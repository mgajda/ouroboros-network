{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}

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
import           Data.List as L
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
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Common
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Subscription.Subscriber
import           Ouroboros.Network.Time

import qualified Test.Mux as Mxt

--
-- The list of all tests
--

tests :: TestTree
tests =
    testGroup "Subscription"
        [
         --testProperty "subscription demo"        demo
          testProperty "Resolve (Sim)"  prop_resolv_sim
        -- , testProperty "Resolve (IO)"  prop_resolv_io
        -- ^ takes about 10 minutes to run due to delays in realtime.
        ]

data LookupResult = LookupResult {
      ipv4Result    :: !(Either DNS.DNSError [IP.IPv4])
    , ipv4Delay     :: !DiffTime
    , ipv6Result    :: !(Either DNS.DNSError [IP.IPv6])
    , ipv6Delay     :: !DiffTime
    , connectionRtt :: !DiffTime
    }

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
    show a = printf "LookupResult: ipv4: %s delay %s ipv6: %s delay %s rtt %s" (show $ ipv4Result a)
                    (show $ ipv4Delay a) (show $ ipv6Result a) (show $ ipv6Delay a)
                    (show $ connectionRtt a)

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
      ipv4d <- choose (0, 3000)
      ipv6r <- arbitrary
      ipv6d <- choose (0, 3000)
      conrtt <- choose (0, 250)

      let minDistance = 10 -- 10ms minimum time between IPv4 and IPv6 result.

      {-
       - For predictability we don't generate lookup results that are closer than 10ms to
       - each other. Since 10ms is still less than resolutionDelay we can still test that
       - behaviour related to resolutionDelay works correctly.
       -}
      let (ipv4d', ipv6d') = if abs (ipv4d - ipv6d) < minDistance
                                 then if ipv4d > ipv6d then (ipv4d + minDistance, ipv6d)
                                                       else (ipv4d, ipv6d + minDistance)
                                 else (ipv4d, ipv6d)
      return $ LookupResult ipv4r (microsecondsToDiffTime $ 1000 * ipv4d') ipv6r
                            (microsecondsToDiffTime $ 1000 * ipv6d')
                            (microsecondsToDiffTime $ 1000 * conrtt)

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
    say $ printf "%s" $ show lr
    let resolver = mockResolver lr
    x <- dnsResolve resolver $ DnsSubscriptionTarget "shelley-1.iohk.example" 1 2
    !res <- checkResult <$> dumpResult x []

    {-
     - We wait 100ms here so that the resolveAAAA and resolveA thread have time to
     - exit, otherwise runSimStrictShutdown will complain about thread leaks.
     -
     - Change dnsResolv to return the two Asyncs so we can wait on them?
     -}
    threadDelay 0.1
    return res

  where
    checkResult :: [Socket.SockAddr] -> Property
    checkResult addrs =
        case (ipv4Result lr, ipv6Result lr) of
            (Left _, Left _)   -> property $ null addrs

            (Right [], Right [])   -> property $ null addrs

            (Right ea, Left _) ->
                -- Expect a permutation of the result of the A lookup.
                property $ permCheck addrs $ map (\ip -> Socket.SockAddrInet 1
                        (IP.toHostAddress ip)) ea

            (Left _, Right ea) ->
                -- Expect a permutation of the result of the AAAA lookup.
                property $ permCheck addrs $ map (\ip -> Socket.SockAddrInet6 1 0
                    (IP.toHostAddress6 ip) 0) ea

            (Right addrs4, Right addrs6) ->
                let sa4s = map ipToSockAddr  addrs4
                    sa6s = map ipToSockAddr6 addrs6
                    (cntA, cntB, headFamily) =
                        if addrs4 /= [] && (ipv4Delay lr + resolutionDelay < ipv6Delay lr
                                        || addrs6 == [])
                            then (length addrs4, length addrs6, Socket.AF_INET)
                            else (length addrs6, length addrs4, Socket.AF_INET6) in
                property $ permCheck addrs (sa4s ++ sa6s) &&
                        sockAddrFamily (head addrs) == headFamily &&
                        alternateFamily addrs (sockAddrFamily (head addrs)) True
                            cntA cntB

    -- Once both the A and the AAAA lookup has returned the result should
    -- alternate between the address families until one family is out of addresses.
    -- This means that:
    -- AAAABABABABABABBB is a valid sequense.
    -- AAAABABAAABABABBB is not a valid sequense.
    alternateFamily :: [Socket.SockAddr] -> Socket.Family -> Bool -> Int -> Int -> Bool
    alternateFamily []       _  _    _    _    = True
    alternateFamily _       _  _     (-1)  _   = False
    alternateFamily _       _  _     _    (-1) = False
    alternateFamily (sa:sas) fa True cntA cntB =
        if sockAddrFamily sa == fa
            then alternateFamily sas fa True (cntA - 1) cntB
            else alternateFamily sas (sockAddrFamily sa) False (cntB - 1) cntA
    alternateFamily (sa:sas) fa False cntA cntB =
        if sockAddrFamily sa == fa
            then if cntB == 0 then alternateFamily sas fa False (cntA - 1) cntB
                              else False
            else alternateFamily sas (sockAddrFamily sa) False (cntB - 1) cntA

    ipToSockAddr6 ip = Socket.SockAddrInet6 1 0 (IP.toHostAddress6 ip) 0
    ipToSockAddr  ip = Socket.SockAddrInet  1   (IP.toHostAddress ip)

    -- | Return true if  `a` is a permutation of `b`.
    permCheck :: [Socket.SockAddr] -> [Socket.SockAddr] -> Bool
    permCheck a b = L.sort a == L.sort b

    dumpResult :: SubscriptionTarget m Socket.SockAddr -> [Socket.SockAddr] -> m [Socket.SockAddr]
    dumpResult targets addrs = do
        target_m <- getSubscriptionTarget targets
        case target_m of
             Just (addr, nextTargets) -> do
                 say $ printf "%s" $ show addr
                 threadDelay (connectionRtt lr)
                 dumpResult nextTargets (addr:addrs)
             Nothing -> do
                 say $ printf "done"
                 return $ reverse addrs

prop_resolv_sim :: LookupResult -> Property
prop_resolv_sim lr =
    case runSimStrictShutdown $ prop_resolv lr of
         Left _  -> property False
         Right r -> r

prop_resolv_io :: LookupResult -> Property
prop_resolv_io lr = ioProperty $ prop_resolv lr


--
-- Properties
--


{-
 - XXX Doesn't really test anything, doesn't exit in a resonable time.
 - XXX Depends on external network config
 - unbound DNS config example:
local-data: "shelley-1.iohk.example. IN A 192.168.1.115"
local-data: "shelley-1.iohk.example. IN A 192.168.1.215"
local-data: "shelley-1.iohk.example. IN A 192.168.1.216"
local-data: "shelley-1.iohk.example. IN A 192.168.1.100"
local-data: "shelley-1.iohk.example. IN A 192.168.1.101"
local-data: "shelley-1.iohk.example. IN A 127.0.0.1"
local-data: "shelley-1.iohk.example. IN AAAA ::1"

local-data: "shelley-0.iohk.example. IN AAAA ::1"
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
                                            --    DnsSubscriptionTarget "shelley-0.iohk.example" 6064 1
                                             DnsSubscriptionTarget "shelley-1.iohk.example" 6062 2
                                            --, DnsSubscriptionTarget "shelley-9.iohk.example" 6066 1
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

