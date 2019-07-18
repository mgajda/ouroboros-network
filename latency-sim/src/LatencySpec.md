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
               . assureAtLeastOneElement . dropTrailingZeros
               . unSeries                . prob
  where
    assureAtLeastOneElement []    = [0.0]
    assureAtLeastOneElement other = other
    dropTrailingZeros             = reverse . dropWhile (==0.0) . reverse
```

We use QuickCheck generate random distribution for testing:
```{.haskell .literate}
instance Arbitrary LatencyDistribution where
  arbitrary = sized $ \maxLen -> do
    actualLen <- choose (0, maxLen-1)
    ld <- LatencyDistribution . Series <$> case actualLen of
            0 -> (:[]) <$> arbitrary
            _ -> -- Last should be non-zero
                 (++) <$> vector actualLen
                      <*> ((:[]) . getPositive <$> arbitrary)
    if isValidLD ld
       then pure ld
       else arbitrary
  shrink (unSeries . prob -> ls) = LatencyDistribution <$> Series <$> recursivelyShrink ls
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
