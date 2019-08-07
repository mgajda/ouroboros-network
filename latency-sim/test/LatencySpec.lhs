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
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}
module LatencySpec where

import GHC.Exts(IsList(..))
import Data.Function(on)
import Data.Monoid((<>))
import Data.Typeable
import Data.Validity
import Data.GenValidity

import Latency
import Metric
import Probability
import Series

import ProbabilitySpec
import SeriesSpec

import Test.QuickCheck.Gen(sized, choose)
import Test.QuickCheck((==>))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.Validity.Arbitrary
import Test.Validity.Eq
import Test.Validity.Show
import Test.Validity.Shrinking

import Test.Hspec.QuickCheck(prop)
import Test.Hspec(describe, it, shouldBe, shouldSatisfy, Spec, SpecWith)
import Test.Hspec.Expectations(expectationFailure, Expectation, HasCallStack)

```

# Appendix: Validation and test case generation for latency distributions

Note that we assume that correct distribution is a list of at least one,
and is devoid of superfluous trailing zeros beyond first index.
Indexing starts at 0 (which means: no delay.)
```{.haskell .literate}
-- | Validity criteria for latency distributions
isValidLD :: LatencyDistribution -> Bool
isValidLD [ ] = False
```
Any distribution with a single-element domain is valid (even if the value is 0.0)
```{.haskell .literate}
isValidLD [p] = isValidProbability p
```
Any distribution with more than one element and last element of zero is invalid (to prevent redundant representations.)
```{.haskell .literate}
isValidLD (last . unSeries . pdf -> 0.0) = False
```
Sum of probabilities shall never exceed 1.0
```{.haskell .literate}
isValidLD ((1.0<) . sum . pdf -> True) = False
```
Each value must be valid value for probability:
```{.haskell .literate}
isValidLD (pdf -> probs) = all isValidProbability probs
```

We use QuickCheck [@quickcheck] to generate random distribution for testing:
```{.haskell .literate}
instance Arbitrary LatencyDistribution where
  arbitrary = sized $ \maxLen -> do
    actualLen <- choose (0, maxLen-1)
    ld <- canonicalizeLD . LatencyDistribution . Series <$>
            case actualLen of
              0 -> (:[]) <$> arbitrary
              _ -> -- Last should be non-zero
                   (++) <$> vector actualLen
                        <*> ((:[]) . getPositive <$> arbitrary)
    if isValidLD ld
       then pure ld
       else arbitrary
  shrink (unSeries . pdf -> ls) =
    filter isValidLD
          (LatencyDistribution <$>
           Series              <$>
           recursivelyShrink       ls)

deriving instance Validity     LatencyDistribution
deriving instance GenValid     LatencyDistribution
deriving instance GenUnchecked LatencyDistribution
deriving instance Typeable     LatencyDistribution
```
Equality uses lexicographic comparison, since that allows shortcut evaluation
by the length of shorter distribution:
```{.haskell .literate}
instance Eq LatencyDistribution where
  bs == cs = (lexCompare `on` (unSeries . pdf)) bs cs == EQ

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

```{.haskell .literate .hidden}
-- | Hspec expectation for showing instance when they are too dissimilar.
shouldBeSimilar :: (HasCallStack
                   ,TimeToCompletion ttc
                   ,Metric           ttc
                   ,Show             ttc
                   ) => ttc -> ttc -> Expectation
a `shouldBeSimilar` b =
   if (a~~b)
     then True `shouldBe` True
     else expectationFailure msg
  where
    dist = a `distance` b
    msg = "Expected: " <> show a <> "\nActual: " <> show b <> "\ndifference is:" <> show (a `distance` b)

infix 3 `shouldBeSimilar`
```
Knowing how similar distributions look like, we should formulate laws of
`TimeToCompletion` as QuickCheck [@quickcheck] specifications:
```{.haskell .literate}
lawsOfTTC :: forall a. (TimeToCompletion a
                       ,Eq               a
                       ,Arbitrary        a
                       ,Show             a
                       ,Metric           a)
          => SpecWith ()
lawsOfTTC = do
  it "noDelay is the same as delay 0" $ (noDelay :: a) `shouldBe` delay 0
  describe "basic laws of convolution" $ do
    prop "commutative"        $ \l m   ->  l `after` m `shouldBeSimilar` m `after` (l :: a)
    prop "associative"        $ \l m n -> (l `after` m) `after` (n :: a)
                                       ~~  l `after` (m `after` n)
    prop "delay 0 is neutral" $ \l     ->  l `after` delay 0
                                       ~~ (l :: a)
  describe "basic laws of firstToFinish" $ do
    prop "commutative"        $ \l m   -> l `firstToFinish` m
                                       ~~ m `firstToFinish` (l :: a)
    prop "associative"        $ \l m n -> (l `firstToFinish` m) `firstToFinish` (n :: a)
                                       ~~  l `firstToFinish` (m `firstToFinish` n)
    prop "delay 0 is neutral" $ \l     ->  l `firstToFinish` allLost
                                       ~~ (l :: a)
  describe "basic laws of firstToFinish" $ do
    prop "commutative"        $ \l m   -> l `lastToFinish` m
                                       ~~ m `lastToFinish` (l :: a)
    prop "associative"        $ \l m n -> (l `lastToFinish` m) `lastToFinish` (n :: a)
                                       ~~  l `lastToFinish` (m `lastToFinish` n)
    prop "delay 0 is neutral" $ \l     ->  l `lastToFinish` delay 0
                                     ~~ (l :: a)
```

```{.haskell .literate}
spec = do
  describe "Arbitrary instance for LatencyDistributions" $ do
    prop "generates result that passes validLD" $ \x -> isValidLD x `shouldBe` True
    it "[0.0, 0.0] is not valid" $ isValidLD [0.0, 0.0] `shouldBe` False
    it "[1.0] is valid"          $ isValidLD [     1.0] `shouldBe` True
    it "[0.0,1.0] is valid"      $ isValidLD [0.0, 1.0] `shouldBe` True
    it "[1.0,0.0] is not valid"  $ isValidLD [1.0, 0.0] `shouldBe` False
    it "isValidLD [0.1, 0.3, 0.2, 0.4]" $ [0.1, 0.3, 0.2, 0.4] `shouldSatisfy` isValidLD
    it "canonicalizeLD [0.1, 0.3, 0.2, 0.4]" $
       canonicalizeLD [0.1, 0.3, 0.2, 0.4] `shouldSatisfy` isValidLD
    prop "canonicalizeLD always produces a valid LatencyDistribution" $
      \s  -> isValidLD (canonicalizeLD (LatencyDistribution (Series s)))
    prop "shrink always produces a valid LatencyDistribution"         $
      \ld -> isValidLD ld ==> all isValidLD (shrink ld)
  describe "basic operations on LatencyDistribution" $ do
    prop "multiplication of to undelayed singletons is preserved by `after`" $
      \          a            b  -> [a] `after`                [b]
                 `shouldBeSimilar` ([a*b] :: LatencyDistribution)
    prop "afterLD of different length distributions is correct"              $
      \(Positive a) (Positive b) -> [a] `after`           [0.0, b]
                 `shouldBeSimilar` ([0.0, a*b] :: LatencyDistribution)
    it   "firstToFinish of different length distributions is correct"        $                               [1.0] `firstToFinish` [0.0, 1.0]
                 `shouldBeSimilar` ([1.0] :: LatencyDistribution)
    it   "lastToFinish of different length distributions is correct"         $                               [1.0] `lastToFinish`  [0.0, 1.0]
                 `shouldBeSimilar` ([0.0, 1.0] :: LatencyDistribution)
    it "noDelay is the same as delay 0" $ (noDelay :: LatencyDistribution) == delay 0
  describe "basic laws of convolution" $ do
    prop "commutative"        $ \l m   ->  l `after` m
                                       ~~ m `after` (l :: LatencyDistribution)
    prop "associative"        $ \l m n -> (l `after` m) `after` (n :: LatencyDistribution)
                                       ~~  l `after` (m `after` n)
    prop "delay 0 is neutral" $ \l     ->  l `after` delay 0
                                       ~~ (l :: LatencyDistribution)
  describe "basic laws of firstToFinish" $ do
     prop "commutative"        $ \l m   -> l `firstToFinish` m
                                        ~~ m `firstToFinish` (l :: LatencyDistribution)
     prop "associative"        $ \l m n ->
           (l `firstToFinish` m) `firstToFinish` (n :: LatencyDistribution)
        ~~  l `firstToFinish` (m `firstToFinish` n)
     prop "delay 0 is neutral" $ \l     ->  l `firstToFinish` allLost
                                        ~~ (l :: LatencyDistribution)
  describe "basic laws of firstToFinish" $ do
     prop "commutative"        $ \l m   -> l `lastToFinish` m
                                        ~~ m `lastToFinish` (l :: LatencyDistribution)
     prop "associative"        $ \l m n -> (l `lastToFinish` m) `lastToFinish` (n :: LatencyDistribution)
                                        ~~  l `lastToFinish` (m `lastToFinish` n)
     prop "delay 0 is neutral" $ \l     ->  l `lastToFinish` delay 0
                                        ~~ (l :: LatencyDistribution)
  eqSpecOnValid   @(Series Int)
  showReadSpec    @(Series Int)
  shrinkValidSpec @(Series Int)
  arbitrarySpec   @(Series Int)

  lawsOfTTC       @LatencyDistribution

```
