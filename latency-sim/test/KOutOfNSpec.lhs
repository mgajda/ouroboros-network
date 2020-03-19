---
input: markdown+tex_math_dollars+yaml_metadata_block+citations
output:
  pdf_document:
    keep_tex: true
    toc: true
    toc_depth: 2
    latex_engine: xelatex
bibliography:
  - Latency.bib
---

```{.haskell .hidden}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KOutOfNSpec where

import Data.Foldable(toList)
import Data.List(iterate')
import Data.Proxy
import GHC.TypeLits(KnownNat)

import KOutOfN
import Latency
import Network()
import NullUnit
import Probability
import Series
import SMatrix

import LatencySpec
import ProbabilitySpec
import SMatrixSpec hiding((|*|))

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Gen(sized, choose, generate, resize, Gen)
import Test.QuickCheck((==>),Property)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.Validity.Arbitrary
import Test.Validity.Eq
import Test.Validity.Show
import Test.Validity.Shrinking
```

First we check that results from `kOutOfN` for no-delay Bernoulli trials are
correct:
```{.haskell .literate}
-- Test instance for immediate (undelayed) outcome
instance TimeToCompletion IdealizedProbability where
  a `firstToFinish` b = a + b - a * b
  a `lastToFinish`  b = a * b
  delay             _ = error
    ( "Delay undefined for TimeToCompletion "
   <> "Probability instance" )
  allLost             = 0
  a `after`         b = a * b

instance TimeToCompletion
           ApproximateProbability where
  a `firstToFinish` b = a + b - a * b
  a `lastToFinish`  b = a * b
  delay             _ = error
     ( "Delay undefined for TimeToCompletion "
    <> "Probability instance" )
  allLost             = 0
  a `after`         b = a * b

nOverK :: Integer -> Integer -> Integer
n `nOverK` k = factorial n `div` factorial k
                           `div` factorial (n-k)

factorial (n::a) = product ([1..n]::[a])

bernoulliSpec ::        IdealizedProbability
   -> Integer -> Series IdealizedProbability
bernoulliSpec probSuccess numTrials =
   Series (bern <$> [0..numTrials])
  where
    bern kSuccesses =
        fromInteger (numTrials `nOverK` kSuccesses)
                  * (probSuccess
                    ^ fromInteger kSuccesses)
                  * (complement probSuccess
                    ^ fromInteger mFailures )
      where
        mFailures = numTrials - kSuccesses
```

Now we can express it as a QuickCheck property:
```{.haskell .literate}
bernoulliProperty p (Positive n) =
     kOutOfN (Series (replicate (fromInteger n) p))
  == bernoulliSpec p (n::Integer)
```

```{.haskell .hidden}

lengthOfKOutOfNIncreasesByOne :: Series ApproximateProbability
                     -> Property
lengthOfKOutOfNIncreasesByOne ser =
    (size ser >= 1) ==> (size ser+1 == size (kOutOfN ser))
  where
    size (Series s) = length s

lengthOfAveragedIncreasesByOne ::
     SomeSMatrix (LatencyDistribution ApproximateProbability)
  -> Bool
lengthOfAveragedIncreasesByOne m = case m of
  SomeSMatrix mat -> length (rows           mat) + 1
                  == length (averageKOutOfN mat)

(|*|) :: KnownNat n
      => SMatrix n (LatencyDistribution IdealizedProbability)
      -> SMatrix n (LatencyDistribution IdealizedProbability)
      -> SMatrix n (LatencyDistribution IdealizedProbability)
(|*|) = sMatMult firstToFinish Latency.after

(***) :: KnownNat n
      => SMatrix n (LatencyDistribution ApproximateProbability)
      -> SMatrix n (LatencyDistribution ApproximateProbability)
      -> SMatrix n (LatencyDistribution ApproximateProbability)
(***) = sMatMult firstToFinish Latency.after

spec = do
  describe "showing surfaces" $ do
    it "showPoint" $
      showPoint 1 2 3 "" `shouldBe` "1 2 3"
    it "showCurve" $
      showCurve 1 [0.5,0.25] "" `shouldBe` "1 0 0.5\n1 1 0.25"
  describe "k-out-of-n" $ do
    prop "bernoulli" bernoulliProperty
    prop "length"    lengthOfKOutOfNIncreasesByOne
    it "tests saving a surface" $ do
      connMatrix :: SMatrix 20 (LatencyDistribution ApproximateProbability)
                 <- generate
                  $ resize 10 genConnMatrix
      putStrLn $ "Dim of input matrix" ++ showDim connMatrix
      let avg = averageKOutOfN connMatrix
      putStrLn $ "Dim of avg " ++ showDim avg
      saveSurface "input_surface.txt" avg
      let iterated = iterate' (***connMatrix) connMatrix !! 5
      putStrLn $ "Dim of iterated " ++ showDim iterated
      saveSurface "result.txt" $ averageKOutOfN iterated
  describe "averageKOutOfN" $ do
    prop "length" lengthOfAveragedIncreasesByOne
    {-prop "isValid after averaging" $ \l -> do
    --    all isValidLD (l :: SMatrix 2 (LatencyDistribution IdealizedProbability))
    -- ==> all isValidLD (averageKOutOfN l)
    prop "isValid after averaging 2" $ do
      l :: SMatrix 2 (LatencyDistribution IdealizedProbability) <- genConnMatrix
      return $ all isValidLD $ averageKOutOfN l-}

showDim :: (Show       a
           ,Foldable f  )
        => f (LatencyDistribution a)
        -> String
showDim f = show $ maximum $ map ldSize $ toList f
  where
    ldSize :: Show a => LatencyDistribution a -> Int
    ldSize (LatencyDistribution (Series s)) = length s
```

To generate connection matrix of size $n$:
```{.haskell .literate}
genConnMatrix :: (KnownNat    n
                 ,Probability                a
                 ,Unit                       a
                 ,Arbitrary                  a)
              => Gen (SMatrix n
                        (LatencyDistribution a))
genConnMatrix =
    sequenceA $ sMatrix Proxy genConnElt
  where
    genConnElt (i, j) | i == j = pure unitE
    genConnElt _               = do
      l@(LatencyDistribution (Series ser))
        <- arbitrary
      case ser of
        [0.0] -> return l -- allow firewalls
        -- If not a firewall, assure that it
        -- has non-zero delay
        _     -> return $ LatencyDistribution
                        $ Series $ 0.0:ser
```
