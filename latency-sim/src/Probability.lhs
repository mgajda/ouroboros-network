---
author:
  - Michał J. Gajda
  - Karl      Knutsson
  - Duncan    Coutts
  - Marcin    Szamotulski
title: Curious properties of latency distributions
abstract: |
  Network latency distributions, their algebra, and use examples.
date: June 25 2019, v1.9
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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}
module Probability(IdealizedProbability(..)
                  ,isValidIdealizedProbability
                  ,ApproximateProbability(..)
                  ,isValidApproximateProbability
                  ,Probability(..)
                  ,Complement(..)
                  ) where

import Data.Ratio((%), Ratio)

import Metric
```

# Appendix: Probability data type

<a name="probability">Probability</a>
is defined here for reference:
```{.haskell .literate}
-- | Between 0.0 and 1.0
newtype IdealizedProbability = IProb { unIProb :: Ratio Integer }
  deriving (Num, Fractional, Real, Ord, Eq,
            Read, Enum)

class (Complement a
      ,Fractional a
      ,Num        a
      ,Eq         a
      ,Ord        a
      ,Show       a
      ,Enum       a
      ) => Probability a where
  isValidProbability :: a -> Bool

instance Show IdealizedProbability where
  showsPrec p (IProb x) = showsPrec p $ realToFrac x

isValidIdealizedProbability :: IdealizedProbability -> Bool
isValidIdealizedProbability p = p>= 0 && p<=1.0

newtype ApproximateProbability = AProb { unAProb :: Double }
  deriving (Num, Fractional, Real, Ord, Eq,
            Read, Enum)

instance Show ApproximateProbability where
  showsPrec p (AProb x) = showsPrec p x

instance Metric ApproximateProbability where
  AProb a `distance` AProb b = abs (a-b)
  similarityThreshold = 0.001

instance Probability IdealizedProbability where
  isValidProbability = isValidIdealizedProbability

instance Probability ApproximateProbability where
  isValidProbability = isValidApproximateProbability

isValidApproximateProbability p = p>=0 && p<=1.0+epsilon
  where
    epsilon = 1e-12
```

For probability, there is a well established definition of a `complement`:
```{.haskell .literate}
instance Complement IdealizedProbability where
  complement p = 1-p

instance Complement ApproximateProbability where
  complement p = 1-p
```
### Definition of complement
In different contexts we use a different definition of the complement.
```{.haskell .literate}
class Complement a where
  complement :: a -> a
```
Here we put the definitions.
