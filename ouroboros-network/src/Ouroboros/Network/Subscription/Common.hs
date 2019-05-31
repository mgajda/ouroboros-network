{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Subscription.Common
    ( subscribeTo
    ) where

import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise)
import           Control.Concurrent.Async
import           Control.Concurrent hiding (threadDelay)

import           Control.Monad (when)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Data.List (delete)
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.Word
import qualified Network.Socket as Socket
import           Text.Printf

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Mux.Interface
import           Ouroboros.Network.Socket (connectTo')
import           Ouroboros.Network.Subscription.Subscriber


-- | Time to wait between connection attempts.
connectionAttemptDelay :: DiffTime
connectionAttemptDelay = 0.250 -- 250ms delay
--connectionAttemptDelay = 0.00250 -- 0.250ms delay

sockAddrFamily :: Socket.SockAddr -> Socket.Family
sockAddrFamily (Socket.SockAddrInet  _ _    ) = Socket.AF_INET
sockAddrFamily (Socket.SockAddrInet6 _ _ _ _) = Socket.AF_INET6
sockAddrFamily _ = error "unsupported socket family"

data ConnectResult = ConnectSuccess
                   | ConnectSuccessLast
                   | ConnectFail
                   deriving (Eq, Ord)

subscribeTo
    :: forall ptcl vNumber extra.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     , Show ptcl
     , Mx.MiniProtocolLimits ptcl
     )
    => Socket.PortNumber
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> Versions vNumber extra (MuxApplication InitiatorApp ptcl IO)
    -> TVar IO Word16
    -> SubscriptionTarget IO Socket.SockAddr
    -> IO ()
subscribeTo localPort encodeData decodeData versions valencyVar ts = do
     x <- newTVarM []
     doSubscribeTo x ts

  where
    doSubscribeTo :: TVar IO [ThreadId] -> SubscriptionTarget IO Socket.SockAddr -> IO ()
    doSubscribeTo conThreads targets = do
        target_m <- getSubscriptionTarget targets
        case target_m of
            Just (target, nextTargets) -> do
                valencyLeft <- atomically $ readTVar valencyVar
                if valencyLeft > 0
                    then do
                        printf "going to subscribe to %s\n" (show target)
                        caid <- async $ doConnect conThreads target
                        atomically $ modifyTVar' conThreads (\t -> ((asyncThreadId caid):t))
                        threadDelay connectionAttemptDelay
                        doSubscribeTo conThreads nextTargets
                    else do
                        printf "successfully started required subscriptions\n"
                        return ()
            Nothing -> do
                printf "out of targets, waiting on active connections\n"

                -- We wait on the list of active connection threads instead of using an async wait function
                -- since some of the connections may succed and then should be left running.
                atomically $ do
                    activeCons  <- readTVar conThreads
                    if activeCons == [] then return ()
                                        else retry
                printf "done waiting on actice connections\n"
                return ()


    doConnect conThreads remoteAddr = do
        bracket
            ( do
                sd <- Socket.socket (sockAddrFamily remoteAddr) Socket.Stream Socket.defaultProtocol
                hasRefVar <- newTVarM False
                return (sd, hasRefVar)
            )
            (\(sd,hasRefVar) -> do
                tid <- myThreadId
                printf "%s: dc bracket cleaning\n" $ show tid
                Socket.close sd

                atomically $ modifyTVar' conThreads (delete tid)
                hasRef <- atomically $ readTVar hasRefVar
                when hasRef $
                    atomically $ modifyTVar' valencyVar (\a -> a + 1)
                return ()
            )
            (\(sd,hasRefVar)  -> do
                Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
                Socket.setSocketOption sd Socket.ReusePort 1
#endif
                let localAddr = case sockAddrFamily remoteAddr of
                                        Socket.AF_INET  -> Socket.SockAddrInet localPort 0
                                        Socket.AF_INET6 -> Socket.SockAddrInet6 localPort 0 (0,0,0,0) 0
                                        _               -> error "unkown addrFamily" -- XXX
                printf "ready to bind\n"
                Socket.bind sd localAddr
                res <- try $ Socket.connect sd remoteAddr
                case res of
                        Left (e :: SomeException) ->  printf "connected failed with %s\n" (show res) >> throwM e
                        Right _ -> printf "connected\n"

                -- We successfully connected, increase valency and start the app
                tid <- myThreadId
                result <- atomically $ do
                        v <- readTVar valencyVar
                        if v > 0 then do
                                    modifyTVar' valencyVar (\a -> a - 1)
                                    writeTVar hasRefVar True
                                    modifyTVar' conThreads (delete tid)
                                    if v == 1 then return ConnectSuccessLast
                                              else return ConnectSuccess
                                else return ConnectFail
                left <- atomically $ readTVar valencyVar
                printf "connected to %s, left %d\n" (show remoteAddr)  left
                case result of
                    ConnectSuccess -> do
                            threadDelay 120 -- XXX
                            r <- try $ connectTo' encodeData decodeData versions sd
                            case r of
                                 Left (e :: SomeException) ->  printf "connectTo' failed with %s\n" (show r) >> throwM e
                                 Right _ -> printf "app exited\n"

                    ConnectSuccessLast -> do
                            outstandingConThreads <- atomically $ readTVar conThreads
                            printf "killing of %s\n" (show outstandingConThreads)
                            mapM_ (\a -> throwTo a (SubscriberError "Parrallel connection timeout")) outstandingConThreads
                            threadDelay 120 -- XXX
                            connectTo' encodeData decodeData versions sd
                    ConnectFail -> do
                        printf "not trying to connect to %s\n" (show remoteAddr)
                        return ()

            )


data SubscriberError = SubscriberError String deriving Show

instance Exception SubscriberError where
    displayException (SubscriberError msg) = printf "Subscriber error %s" msg

