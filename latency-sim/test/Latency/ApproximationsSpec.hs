{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Latency.ApproximationsSpec(spec) where

import Latency

import Test.QuickCheck
import Test.QuickCheck.All
import Test.Hspec
import Test.Hspec.QuickCheck

import GHC.Exts(IsList(..))

skip _ = pure ()

spec :: Spec
spec = do
         describe "Check individual ops" $ do
           describe "TTC ops on earliest" $ do
             it "earliest of noDelay is 0" $ do
               earliest [1] == Earliest 0
             it "earliest of [0.0] is 0" $ do
               earliest [0.0] == Earliest 0
             it "earliest of [1.0] is 0" $ do
               earliest [1.0] == Earliest 0
             it "earliest is never lower than 0" $
               property $ \x -> earliest x >= Earliest 0
             it "earliest of single element series is always 0" $
               property $ \x -> earliest [x] == Earliest 0
             it "earliest of n element series of non-zero elements is always zero" $
               property $ \(Positive x) (Positive n) -> earliest (fromList $ replicate n x) == Earliest 0
             skip $ it "earliest of allLost is 0" $ do
               earliest allLostLD == Earliest 0
             it "earliest of delay t is t" $ do
               property $ \t -> earliest (delay t) == Earliest (t :: Delay)
           describe "TTC ops on latest" $ do
             it "latest of noDelay is 0" $ do
               latest [1] == Latest 0
             skip $ it "Latest of allLost is 0" $ do
               latest allLostLD == Latest 0
             it "latest of [0.0] is 0" $ do
               latest [0.0] == Latest 0
             it "latest of delay t is t" $ do
               property $ \t -> latest (delay t) == Latest (t :: Delay)
             it "latest of series with indices 0, 1 is one" $
               property $ latest [0.0, 1.0] == Latest 1
             it "latest of series with indices 0, 1 is one" $
               property $ \n -> latest (fromList [0.1 | _ <- [0..n]]) == Latest n
             it "earliest of n element series of non-zero elements is always its length less one" $
               property $ \(Positive x) (NonNegative n) -> latest (fromList $ replicate n x) == Latest (Delay (n-1))
         describe "Check approximations that are functors" $ do
           it "earliest is a functor" earliestIsTTCFunctor
           it "earliest is a functor" latestIsTTCFunctor

earliestIsTTCFunctor = property $ verifyTTCFunctor (\a b -> earliest a == b) earliest

latestIsTTCFunctor   = property $ verifyTTCFunctor (\a b -> latest   a == b) latest


