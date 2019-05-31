{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies               #-}


module Ouroboros.Network.Subscription.Dns
    ( DnsSubscriptionTarget (..)
    , Resolver (..)
    , dnsSubscriptionWorker
    , dnsResolve
    ) where

import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise)
import           Control.Monad (forever, when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import qualified Network.DNS as DNS
import           Data.Functor (void)
import qualified Data.IP as IP
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.Word
import qualified Network.Socket as Socket
import           Text.Printf

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Mux.Interface
import           Ouroboros.Network.Subscription.Common
import           Ouroboros.Network.Subscription.Subscriber

-- | Time to wait for an AAAA response after receiving an A response.
resolutionDelay :: DiffTime
resolutionDelay = 0.05 -- 50ms delay

-- | Minimum time to wait between dns lookups
dnsRetryDelay :: DiffTime
dnsRetryDelay = 10 -- 10s delay

data DnsSubscriptionTarget = DnsSubscriptionTarget {
      dstDomain :: !DNS.Domain
    , dstPort   :: !Word16
    , dstValeny :: !Word16
    } deriving (Eq, Show)

data Resolver m = Resolver {
      lookupA :: DNS.Domain -> m (Either DNS.DNSError [IP.IPv4])
    , lookupAAAA :: DNS.Domain -> m (Either DNS.DNSError [IP.IPv6])
    }

dnsResolve :: forall m.
     ( MonadAsync m
     , MonadSay   m
     , MonadSTM   m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow m
     )
    => Resolver m
    -> DnsSubscriptionTarget
    -> m (SubscriptionTarget m Socket.SockAddr)
dnsResolve resolver (DnsSubscriptionTarget domain port _) = do
    ipv6Rsps <- newEmptyTMVarM
    ipv4Rsps <- newEmptyTMVarM
    gotIpv6Rsp <- newTVarM False

    aid_ipv6 <- async $ resolveAAAA gotIpv6Rsp ipv6Rsps
    aid_ipv4 <- async $ resolveA gotIpv6Rsp ipv4Rsps

    rd_e <- waitEitherCatch aid_ipv6 aid_ipv4
    case rd_e of
         Left r_ipv6 -> -- AAAA lookup finished first
             case r_ipv6 of
                  Left e_ipv6 -> do -- AAAA lookup failed
                      say $ printf "AAAA lookup failed with %s\n" (show e_ipv6)
                      return $ SubscriptionTarget $ listTargets (Right ipv4Rsps) (Left [])
                  Right _ -> do
                      -- Try to use IPv6 result first.
                      ipv6Res <- atomically $ takeTMVar ipv6Rsps
                      return $ SubscriptionTarget $ listTargets (Left ipv6Res) (Right ipv4Rsps)
         Right r_ipv4 -> do
              case r_ipv4 of
                  Left e_ipv4 -> do -- A lookup failed
                      say $ printf "A lookup failed with %s\n" (show e_ipv4)
                      return $ SubscriptionTarget $ listTargets (Right ipv6Rsps) (Left [])
                  Right _ -> do
                      return $ SubscriptionTarget $ listTargets (Right ipv4Rsps) (Right ipv6Rsps)

  where

    listTargets :: Either [Socket.SockAddr] (TMVar m [Socket.SockAddr])
                -> Either [Socket.SockAddr] (TMVar m [Socket.SockAddr])
                -> m (Maybe (Socket.SockAddr, SubscriptionTarget m Socket.SockAddr))

    -- No result left to try
    listTargets (Left []) (Left []) = return Nothing

    -- No results left to try for one family
    listTargets (Left []) ipvB = listTargets ipvB (Left [])

    -- Result for one address family
    listTargets (Left (addr : addrs)) ipvB =
        return $ Just (addr, SubscriptionTarget (listTargets ipvB (Left addrs)))

    -- No result for either family yet.
    listTargets (Right addrsVarA) (Right addrsVarB) = do
        addrsRes <- atomically $ do
            a_m <- tryReadTMVar addrsVarA
            b_m <- tryReadTMVar addrsVarB
            case (a_m, b_m) of
                 (Nothing, Nothing) -> retry
                 (Just a, _)        -> return $ Left a
                 (_, Just b)        -> return $ Right b
        let (addrs, nextAddrs) = case addrsRes of
                                      Left a  -> (a, Right addrsVarB)
                                      Right a -> (a, Right addrsVarA)
        if null addrs
           then listTargets (Right addrsVarB) (Left [])
           else return $ Just (head addrs, SubscriptionTarget (listTargets nextAddrs (Left $ tail addrs)))

    -- Wait on the result for one family.
    listTargets (Right addrsVar) (Left []) = do
        addrs <- atomically $ takeTMVar addrsVar
        listTargets (Left addrs) (Left [])

    -- Peek at the result for one family.
    listTargets (Right addrsVar) (Left a) = do
        addrs_m <- atomically $ tryTakeTMVar addrsVar
        case addrs_m of
             Just addrs -> listTargets (Left addrs) (Left a)
             Nothing    -> listTargets (Left a) (Right addrsVar)


    resolveAAAA gotIpv6RspVar rspsVar = do
        r_e <- lookupAAAA resolver domain
        case r_e of
             Left e  -> do
                 atomically $ putTMVar rspsVar []
                 atomically $ writeTVar gotIpv6RspVar True
                 return $ Just e
             Right r -> do
                 atomically $ putTMVar rspsVar $
                     map (\ip -> Socket.SockAddrInet6 (fromIntegral port) 0 (IP.toHostAddress6 ip) 0) r
                 atomically $ writeTVar gotIpv6RspVar True
                 return Nothing

    resolveA :: TVar m Bool -> TMVar m [Socket.SockAddr] -> m (Maybe DNS.DNSError)
    resolveA gotIpv6RspVar rspsVar= do
        r_e <- lookupA resolver domain
        case r_e of
             Left e  -> do
                 atomically $ putTMVar rspsVar []
                 return $ Just e
             Right r -> do
                 {- From RFC8305.
                  - If a positive A response is received first due to reordering, the client
                  - SHOULD wait a short time for the AAAA response to ensure that preference is
                  - given to IPv6.
                  -}
                 timeoutVar <- registerDelay resolutionDelay
                 atomically $ do
                     timeout <- readTVar timeoutVar
                     gotIpv6Rsp <- readTVar gotIpv6RspVar
                     if timeout || gotIpv6Rsp
                         then return ()
                         else retry
                 atomically $ putTMVar rspsVar $
                     map (\ip -> Socket.SockAddrInet (fromIntegral port) (IP.toHostAddress ip)) r
                 return Nothing

dnsSubscriptionWorker
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
    -> [DnsSubscriptionTarget]
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> Versions vNumber extra (MuxApplication InitiatorApp ptcl IO)
    -> IO ()
dnsSubscriptionWorker localPort dsts encodeData decodeData versions = do
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf

    mask $ \unmask -> do
        workers <- traverse (async . unmask . (domainWorker rs)) dsts
        unmask (void $ waitAnyCancel workers)

  where
    domainWorker rs dst = void $ DNS.withResolver rs $ \dnsResolver -> do
        valencyVar <- newTVarM (dstValeny dst)
        let resolver = Resolver (DNS.lookupA dnsResolver) (DNS.lookupAAAA dnsResolver)
        forever $ do
            start <- getMonotonicTime
            targets <- dnsResolve resolver (dst)
            subscribeTo localPort encodeData decodeData versions valencyVar targets
            valency <- atomically $ do
                v <- readTVar valencyVar
                if v == 0 then retry
                          else return v
            end <- getMonotonicTime
            let duration = diffTime end start
            printf "%s duration %s valency at %d, connecting to more hosts\n" (show $ dstDomain dst)
                (show duration) valency

            -- We allways wait at least 1s between calls to dnsResolve. This means that if we loose
            -- connecton to multiple targets around the same time we will retry with a desired valancy
            -- that is higher then 1.
            threadDelay 1
            valency' <- atomically $ readTVar valencyVar
            printf "%s valency after sleep %d\n" (show $ dstDomain dst) valency'
            when (duration < dnsRetryDelay - 1) $ do
                printf "%s sleeping an additional %s\n" (show $ dstDomain dst)
                    (show $ dnsRetryDelay - 1 - duration)
                threadDelay $ dnsRetryDelay - 1 - duration
            return ()


