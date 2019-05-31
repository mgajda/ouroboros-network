{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


{-# OPTIONS_GHC -Wno-orphans     #-}

module Test.Subscription (tests) where

import           Control.Monad (replicateM)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim (runSimStrictShutdown)
import           Data.Functor (void)
import qualified Data.IP as IP
import qualified Network.Socket as Socket
import qualified Network.DNS as DNS
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Text.Printf

import           Test.Tasty.QuickCheck (testProperty)


import           Codec.SerialiseTerm
import           Ouroboros.Network.Mux.Interface
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Time

import qualified Test.Mux as Mxt

--
-- The list of all tests
--

tests :: TestTree
tests =
    testGroup "Subscription"
        [
         testProperty "subscription demo"        demo
        ]

data LookupResult = LookupResult {
      ipv4Result :: !(Either DNS.DNSError [IP.IPv4])
    , ipv4Delay  :: !DiffTime
    , ipv6Result :: !(Either DNS.DNSError [IP.IPv6])
    , ipv6Delay  :: !DiffTime
    }

lookupResultIPv4Error :: LookupResult -> Bool
lookupResultIPv4Error lr =
    case ipv4Result lr of
         Left  _ -> True
         Right _ -> False

lookupResultIPv6Error :: LookupResult -> Bool
lookupResultIPv6Error lr =
    case ipv6Result lr of
         Left  _ -> True
         Right _ -> False

mockResolver :: forall m. (MonadTimer m) => LookupResult -> Resolver m
mockResolver lr = Resolver lA lAAAA
  where
    lA :: DNS.Domain -> m (Either DNS.DNSError [IP.IPv4])
    lA    _ = do
        threadDelay (ipv4Delay lr)
        return $ ipv4Result lr

    lAAAA :: DNS.Domain -> m (Either DNS.DNSError [IP.IPv6])
    lAAAA _ = do
        threadDelay (ipv6Delay lr)
        return $ ipv6Result lr

instance Show LookupResult where
    show a = printf "LookupResult: ipv4: %s delay %s ipv6: %s delay %d" (show $ ipv4Result a)
                    (show $ ipv4Delay a) (show $ ipv6Result a) (show $ ipv6Delay a)

instance Arbitrary DNS.DNSError where
    arbitrary = oneof [ return DNS.SequenceNumberMismatch
                      , return DNS.RetryLimitExceeded
                      ]

instance Arbitrary IP.IPv4 where
    arbitrary = do
        a <- replicateM 4 (choose (0,255))
        return $ IP.toIPv4 a

instance Arbitrary IP.IPv6 where
    arbitrary = do
        a <- replicateM 8 (choose (0,0xffff))
        return $ IP.toIPv6 a

instance Arbitrary LookupResult where
    arbitrary = do
      ipv4r <- arbitrary
      ipv4d <- choose (0, 3000000)
      ipv6r <- arbitrary
      ipv6d <- choose (0, 3000000)
      return $ LookupResult ipv4r (microsecondsToDiffTime ipv4d) ipv6r (microsecondsToDiffTime ipv6d)

prop_resolv :: forall m.
     ( MonadAsync m
     , MonadSay   m
     , MonadSTM   m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow m
     )
     => LookupResult
     -> m Property
prop_resolv lr =  do
    let resolver = mockResolver lr
    x <- dnsResolve resolver $ DnsSubscriptionTarget "asdf.com" 1 2
    return $ property True

prop_resolv_sim :: LookupResult -> Property
prop_resolv_sim lr = lookupResultIPv4Error lr && not (lookupResultIPv6Error lr) ==>
    case runSimStrictShutdown $ prop_resolv lr of
         Left _  -> property False
         Right r -> r

--
-- Properties
--


{-
 - XXX Doesn't really test anything, doesn't exit in a resonable time.
 - XXX Depends on external network config
 - unbound DNS config example:
local-data: "shelley-1.iohk. IN A 192.168.1.115"
local-data: "shelley-1.iohk. IN A 192.168.1.215"
local-data: "shelley-1.iohk. IN A 192.168.1.216"
local-data: "shelley-1.iohk. IN A 192.168.1.100"
local-data: "shelley-1.iohk. IN A 192.168.1.101"
local-data: "shelley-1.iohk. IN A 127.0.0.1"
local-data: "shelley-1.iohk. IN AAAA ::1"

local-data: "shelley-0.iohk. IN AAAA ::1"
-}
demo :: Property
demo = ioProperty $ do
    server:_ <- Socket.getAddrInfo Nothing (Just "192.168.1.100") (Just "6062")
    server':_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6062")
    server6:_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "6062")
    server6':_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "6064")

    spawnServer server 10000
    spawnServer server' 10000
    spawnServer server6 100
    spawnServer server6' 45

    _ <- async $ dnsSubscriptionWorker 6061 [
                                            --    DnsSubscriptionTarget "shelley-0.iohk" 6064 1
                                             DnsSubscriptionTarget "shelley-1.iohk" 6062 2
                                            --, DnsSubscriptionTarget "shelley-9.iohk" 6066 1
                                            ]
                                       (\(DictVersion codec) -> encodeTerm codec)
                                       (\(DictVersion codec) -> decodeTerm codec)
                                       (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData 0)
                                       (DictVersion nodeToNodeCodecCBORTerm) appReq)
    threadDelay 130
    -- bring the servers back again
    spawnServer server6 10000
    spawnServer server6' 10000
    threadDelay 1000
    return ()

  where
    spawnServer addr delay =
        void $ async $ withSimpleServerNode addr
            (\(DictVersion codec) -> encodeTerm codec)
            (\(DictVersion codec) -> decodeTerm codec)
            (\(DictVersion _) -> acceptEq)
            (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData 0)
                (DictVersion nodeToNodeCodecCBORTerm) appRsp)
            (\_ -> threadDelay delay)


    appReq = MuxInitiatorApplication (\Mxt.ChainSync1 -> error "req fail")
    appRsp = MuxResponderApplication (\Mxt.ChainSync1 -> error "rsp fail")

