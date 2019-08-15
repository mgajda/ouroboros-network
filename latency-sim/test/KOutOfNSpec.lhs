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
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KOutOfNSpec where

import Data.List(iterate')

import Complement
import KOutOfN
import Latency
import Network()
import Probability
import Series
import SMatrix

import LatencySpec
import ProbabilitySpec
import SMatrixSpec hiding((|*|))

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Gen(sized, choose, generate, resize)
import Test.QuickCheck((==>))
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
instance TimeToCompletion Probability where
  a `firstToFinish` b = a + b - a * b -- either a or  b happened
  a `lastToFinish`  b = a * b -- both   a and b happened
  delay             _ = error "Delay undefined for TimeToCompletion Probability instance"
  allLost             = 0
  a `after`         b = a * b -- both a and b happened

nOverK :: Integer -> Integer -> Integer
n `nOverK` k = factorial n `div` factorial k `div` factorial (n-k)

factorial (n::a) = product ([1..n]::[a])

bernoulliSpec :: Probability -> Integer -> Series Probability
bernoulliSpec probSuccess numTrials =
   Series (bern <$> [0..numTrials])
  where
    bern kSuccesses = fromInteger (numTrials `nOverK` kSuccesses)
                        * (probSuccess            ^ (fromInteger kSuccesses))
                        * (complement probSuccess ^ (fromInteger mFailures ))
      where
        mFailures = numTrials - kSuccesses
```

Now we can express it as a QuickCheck property:
```{.haskell.literate}
bernoulliProperty p (Positive n) = kOutOfN (Series (replicate (fromInteger n) p))
                                == bernoulliSpec (p::Probability) (n::Integer)
```

```{.haskell .hidden}

(|*|) = sMatMult firstToFinish Latency.after
spec = do
  describe "showing surfaces" $ do
    it "withLines" $
      withLines [("Alpha"++), ("Beta"++), ("Gamma"++)] ""
        `shouldBe` "Alpha\nBeta\nGamma"
    it "joins" $
      joins ("Alpha"++) ("Beta"++) "" `shouldBe` "Alpha\nBeta"
    it "showPoint" $
      showPoint showProb 1 2 3 "" `shouldBe` "1 2 3.0"
    it "showCurve" $
      showCurve showProb 1 [0.5,0.25] "" `shouldBe` "1 0 0.5\n1 1 0.25"
  describe "k-out-of-n" $ do
    prop "bernoulli" bernoulliProperty
    it "tests saving a surface" $ do
      connMatrix :: SMatrix 5 LatencyDistribution <- generate
        $ resize testSize arbitrary
      --putStrLn "Generated input matrix"
      saveSurface "input_surface.txt" $ averageKOutOfN connMatrix
      --putStrLn "Saved input surface"
      let iterated = iterate' (|*|connMatrix) connMatrix !! testSize
      --after10 `seq` putStrLn "Iterated 10 times"
      -- print after5
      saveSurface "after10.txt" $ averageKOutOfN iterated
      -- True `shouldBe` True

testSize = 5
```
