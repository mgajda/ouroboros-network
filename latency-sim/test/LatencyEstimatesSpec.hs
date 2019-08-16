{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module LatencyEstimatesSpec(spec) where

import Delay
import Probability
import Series hiding(spec)
import Latency
import LatencySpec hiding(spec)
import LatencyEstimates
import Metric

import Test.QuickCheck
import Test.QuickCheck.All
import Test.Hspec(Spec, SpecWith, describe, it, shouldBe, Expectation)
import Test.Hspec.QuickCheck

import GHC.Exts(IsList(..))

skip _ = pure ()

instance Arbitrary Earliest where
  arbitrary = do
    b <- arbitrary
    Earliest <$> if b
                    then return Never
                    else Sometime <$> arbitrary

instance Metric Earliest where
  Earliest  Never       `distance` Earliest  Never       = 0
  Earliest (Sometime t) `distance` Earliest (Sometime s) = fromIntegral $ unDelay $ abs (s-t)
  Earliest  Never       `distance` Earliest (Sometime _) = fromIntegral $ largeValue
  Earliest (Sometime _) `distance` Earliest  Never       = fromIntegral $ largeValue
  similarityThreshold = 1

largeValue = 2^30

instance Arbitrary Latest where
  arbitrary = do
    b <- arbitrary
    Latest <$> if b
                    then return Never
                    else Sometime <$> arbitrary

instance Metric Latest where
  Latest  Never       `distance` Latest  Never       = 0
  Latest (Sometime t) `distance` Latest (Sometime s) = fromIntegral $ unDelay $ abs (s-t)
  Latest  Never       `distance` Latest (Sometime _) = fromIntegral $ largeValue
  Latest (Sometime _) `distance` Latest  Never       = fromIntegral $ largeValue
  similarityThreshold = 1

spec :: Spec
spec = do
         describe "Check individual ops" $ do
           describe "TTC ops on earliest" $ do
             it "earliest of noDelay is 0" $ do
               earliest [1] `shouldBe` Earliest (Sometime 0)
             it "earliest of [0.0] is never" $ do
               earliest [0.0] `shouldBe` Earliest Never
             it "earliest of [1.0] is 0" $ do
               earliest [1.0] `shouldBe` Earliest (Sometime 0)
             it "earliest is never lower than 0, if there are non-lost packets" $
               property $ \x -> x /= allLost ==> earliest x >= Earliest (Sometime 0)
             it "earliest of non-zero singleton is always 0" $
               property $ \x -> x > 0.0 ==> earliest [x] `shouldBe` Earliest (Sometime 0)
             it "earliest of n element series of non-zero elements is always zero" $
               property $ \(Positive x) (Positive n) -> earliest (fromList $ replicate n x)
                             `shouldBe` Earliest (Sometime 0)
             skip $ it "earliest of allLost is 0" $ do
               earliest allLost `shouldBe` Earliest (Sometime 0)
             it "earliest of delay t is t" $ do
               property $ \t -> earliest (delay t) `shouldBe` Earliest (Sometime (t :: Delay))
           describe "TTC ops on latest" $ do
             it "latest of noDelay is 0" $ do
               latest [1] `shouldBe` Latest (Sometime 0)
             it "Latest of allLost is 0" $ do
               latest allLost  `shouldBe` Latest Never
             it "latest of [0.0] is never" $ do
               latest [0.0] `shouldBe` Latest Never
             it "latest of delay t is t" $ do
               property $ \t -> latest (delay t) `shouldBe` Latest (Sometime t)
             it "latest of delay t is t" $ do
               property $ \t -> latest (delay t) `shouldBe` Latest (Sometime t)
             it "latest of series with indices 0, 1 is one" $
               property $ latest [0.0, 1.0] `shouldBe` Latest (Sometime 1)
         describe "Check that bounds are functors" $ do
           earliestIsFunctorForTTC
           latestIsFunctorForTTC
         describe "Earliest" $ lawsOfTTC @Earliest
         describe "Latest" $ lawsOfTTC @Latest
  where
    earliest :: LatencyDistribution IdealizedProbability -> Earliest
    earliest  = LatencyEstimates.earliest
    latest   :: LatencyDistribution IdealizedProbability -> Latest
    latest    = LatencyEstimates.latest

-- | Verify the functor with respect to TTC operations on `LatencyDistribution`s.
verifyTTCFunctor ::  TimeToCompletion a
                 =>  String
                 -> (LatencyDistribution IdealizedProbability
                       -> a -> Expectation)
                 -> (LatencyDistribution IdealizedProbability -> a)
                 ->  SpecWith ()
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
     prop "allLost"       $ allLost  `compatible` allLost

earliestIsFunctorForTTC = verifyTTCFunctor "earliest"
                                          (\a b -> earliest a `shouldBe` b)
                                            earliest

latestIsFunctorForTTC   = verifyTTCFunctor "latest"
                                          (\a b -> latest   a `shouldBe` b)
                                            latest
