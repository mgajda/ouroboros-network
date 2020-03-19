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
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Probability(IdealizedProbability(..)
                  ,isValidIdealizedProbability
                  ,ApproximateProbability(..)
                  ,isValidApproximateProbability
                  ,Probability(..)
                  ,Complement(..)
                  ) where

import Data.Data
import Data.Typeable
import GHC.Generics

import Data.Ratio(Ratio)

import Metric
import NullUnit
```

# Appendix: Probability data type

<a name="probability">Probability</a>
is defined here for reference:
```{.haskell .literate}
-- | Between 0.0 and 1.0
newtype IdealizedProbability =
        IProb { unIProb :: Ratio Integer }
  deriving (Num, Fractional, Real, Ord, Eq,
            Data, Typeable, Generic, Enum)

class (Complement a
      ,Fractional a
      ,Num        a
      ,Eq         a
      ,Ord        a
      ,Show       a
      ,Enum       a
      ,Null       a
      ,Unit       a
      ) => Probability a where
  isValidProbability :: a -> Bool

instance Show IdealizedProbability where
  showsPrec p (IProb x) = showsPrec p x

instance Unit IdealizedProbability where
  unitE = 1

instance Null IdealizedProbability where
  nullE = 0

isValidIdealizedProbability ::
  IdealizedProbability -> Bool
isValidIdealizedProbability p = p>= 0 && p<=1.0

newtype ApproximateProbability =
        AProb { unAProb :: Double }
  deriving (Num, Fractional, Real, Ord, Eq,
            Data, Typeable, Generic, Enum)

instance Show ApproximateProbability where
  showsPrec p (AProb x) = showsPrec p x

instance Metric ApproximateProbability where
  AProb a `distance` AProb b = abs (a-b)
  similarityThreshold = 0.001

instance Metric IdealizedProbability where
  IProb a `distance` IProb b = abs $
                               realToFrac (a-b)
  similarityThreshold = 0 -- no error allowed

instance Unit ApproximateProbability where
  unitE = 1

instance Null ApproximateProbability where
  nullE = 0

instance Probability IdealizedProbability where
  isValidProbability = isValidIdealizedProbability

instance Probability ApproximateProbability where
  isValidProbability =
    isValidApproximateProbability

isValidApproximateProbability p = p>=0
                               && p<=1.0+epsilon
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
