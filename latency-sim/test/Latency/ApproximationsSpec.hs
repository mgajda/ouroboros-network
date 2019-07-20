{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Latency.ApproximationsSpec(spec) where

import Delay
import Probability
import Series hiding(spec)
import Latency
import LatencySpec hiding(spec)
import LatencyEstimates

import Test.QuickCheck
import Test.QuickCheck.All
import Test.Hspec(Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.QuickCheck

import GHC.Exts(IsList(..))

skip _ = pure ()

spec :: Spec
spec = do
         describe "Check individual ops" $ do
           describe "TTC ops on earliest" $ do
             it "earliest of noDelay is 0" $ do
               earliest [1] `shouldBe` Earliest 0
             it "earliest of [0.0] is never" $ do
               earliest [0.0] `shouldBe`Earliest maxBound
             it "earliest of [1.0] is 0" $ do
               earliest [1.0] `shouldBe`Earliest 0
             it "earliest is never lower than 0" $
               property $ \x -> earliest x >= Earliest 0
             it "earliest of non-zero singleton is always 0" $
               property $ \x -> x > 0.0 ==> earliest [x] `shouldBe`Earliest 0
             it "earliest of n element series of non-zero elements is always zero" $
               property $ \(Positive x) (Positive n) -> earliest (fromList $ replicate n x) `shouldBe`Earliest 0
             skip $ it "earliest of allLost is 0" $ do
               earliest (allLost @LatencyDistribution) `shouldBe`Earliest 0
             it "earliest of delay t is t" $ do
               property $ \t -> earliest (delay t) `shouldBe`Earliest (t :: Delay)
           describe "TTC ops on latest" $ do
             it "latest of noDelay is 0" $ do
               latest [1] `shouldBe`Latest 0
             skip $ it "Latest of allLost is 0" $ do
               latest (allLost @LatencyDistribution) `shouldBe`Latest 0
             it "latest of [0.0] is never" $ do
               latest [0.0] `shouldBe`Latest maxBound
             it "latest of delay t is t" $ do
               property $ \t -> latest (delay t) `shouldBe`Latest (t :: Delay)
             it "latest of series with indices 0, 1 is one" $
               property $ latest [0.0, 1.0] `shouldBe`Latest 1
             it "latest of series with indices 0, 1 is one" $
               property $ \n -> latest (fromList [0.1 | _ <- [0..n]]) `shouldBe`Latest n
             it "earliest of n element series of non-zero elements is always its length less one" $
               property $ \(Positive x) (NonNegative n) -> latest (fromList $ replicate n x) `shouldBe`Latest (Delay (n-1))
         describe "Check approximations that are functors" $ do
           earliestIsTTCFunctor
           latestIsTTCFunctor

-- | Verify the functor with respect to TTC operations on `LatencyDistribution`s.
{-verifyTTCFunctor :: (TimeToCompletion b
                    ,TimeToCompletion a
                    ,Show             a
                    ,Arbitrary        a)
                 =>  String
                 -> (a -> b -> Bool)
                 -> (a -> b) -> SpecWith ()-}
verifyTTCFunctor name compatible extract =
  describe (name ++ " is a functor with respect to ") $ do
     prop "firstToFinish" $ \a b -> (isValidLD a && isValidLD b) ==>
                                (        (a `firstToFinish`         b) `compatible`
                                (extract  a `firstToFinish` extract b))
     prop "lastToFinish"  $ \a b ->
                          ((         a `lastToFinish`         b) `compatible`
                           (extract  a `lastToFinish` extract b))
     prop "after"         $ \a b ->
                          ((a `after`                b) `compatible`
                          (extract  a `after`         extract b))
     prop "delay"         $ \t -> delay t `compatible` delay t
-- FIXME:                  delay                     `compatible` delay

earliestIsTTCFunctor = verifyTTCFunctor "earliest" (\a b -> earliest a `shouldBe`b) earliest

latestIsTTCFunctor   = verifyTTCFunctor "latest"   (\a b -> latest   a `shouldBe`b) latest
