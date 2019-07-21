```{.haskell .hidden}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}
module LatencySpec where

import GHC.Exts(IsList(..))
import Data.Function(on)

import Probability
import Series
import Latency

import Test.QuickCheck.Gen(sized, choose)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers

import Test.Hspec.QuickCheck(prop)
import Test.Hspec(describe, it, shouldBe)
```

# Appendix: Validation and test case generation for latency distributions

Note that we assume that correct distribution is a list of at least one,
and is devoid of superfluous trailing zeros beyond first index.
Indexing starts at 0 (which means: no delay.)
```{.haskell .literate}
-- | Validity criteria for latency distributions
isValidLD :: LatencyDistribution -> Bool
isValidLD []            = False
isValidLD [_]           = True -- any distribution with a single-element domain is valid (even if the value is 0.0)
isValidLD (last . unSeries . prob -> 0.0) = False -- any distribution with more than one element and last element of zero is invalid (to prevent redundant representations.)
isValidLD (prob -> probs) = (sum probs) <= 1.0 -- sum of probabilities shall never exceed 0.0
                         && all isValidProbability probs -- each value must be valid value for probability
```

To convert possibly improper `LatencyDistribution` into its canonical representation:
```{.haskell .literate}
canonicalizeLD :: LatencyDistribution -> LatencyDistribution
canonicalizeLD = LatencyDistribution     . Series
               . assureAtLeastOneElement . dropTrailingZeros . cutWhenSumOverOne 0.0
               . unSeries                . prob
  where
    cutWhenSumOverOne aSum []                  = []
    cutWhenSumOverOne aSum (x:xs) | aSum+x>1.0 = []
    cutWhenSumOverOne aSum (x:xs)              = x:cutWhenSumOverOne (aSum+x) xs
    assureAtLeastOneElement []    = [0.0]
    assureAtLeastOneElement other = other
    dropTrailingZeros             = reverse . dropWhile (==0.0) . reverse
```

We use QuickCheck generate random distribution for testing:
```{.haskell .literate}
instance Arbitrary LatencyDistribution where
  arbitrary = sized $ \maxLen -> do
    actualLen <- choose (0, maxLen-1)
    ld <- canonicalizeLD . LatencyDistribution . Series <$> case actualLen of
            0 -> (:[]) <$> arbitrary
            _ -> -- Last should be non-zero
                 (++) <$> vector actualLen
                      <*> ((:[]) . getPositive <$> arbitrary)
    if isValidLD ld
       then pure ld
       else error $ "Generated " <> show ld
  shrink (unSeries . prob -> ls) = filter isValidLD (LatencyDistribution <$> Series <$> recursivelyShrink ls)
```
Equality uses lexicographic comparison, since that allows shortcut evaluation
by the length of shorter distribution:
```{.haskell .literate}
instance Eq LatencyDistribution where
  bs == cs = (lexCompare `on` (unSeries . prob)) bs cs == EQ

lexCompare :: [Probability] -> [Probability] -> Ordering
lexCompare  xs     []    = if all (==0.0) xs
                           then EQ
                           else LT
lexCompare  []     xs    = invertComparison $ lexCompare xs []
lexCompare (x:xs) (y:ys) = case compare x y of
                          EQ    -> lexCompare xs ys
                          other -> other
invertComparison LT = GT
invertComparison GT = LT
invertComparison EQ = EQ
```

```{.haskell .literate}
spec = describe "Arbitrary instance for LatencyDistributions" $ do
         prop "generates result that passes validLD" $ \x -> isValidLD x `shouldBe` True
         it "[0.0, 0.0] is not valid" $ isValidLD [0.0, 0.0] `shouldBe` False
         it "[1.0] is valid" $ isValidLD [1.0] `shouldBe` True
         it "[0.0,1.0] is valid" $ isValidLD [0.0, 1.0] `shouldBe` True
         it "[1.0,0.0] is not valid" $ isValidLD [1.0, 0.0] `shouldBe` False
         prop "canonicalizeLD always produces a valid LatencyDistribution" $ \s -> isValidLD (canonicalizeLD (LatencyDistribution (Series s)))
```
