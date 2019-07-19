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
module Probability(Probability(..)
                  ,isValidProbability) where

import Data.Ratio((%))
import Test.QuickCheck

```

# Appendix: Probability data type

<a name="probability">Probability</a>
is defined here for reference:
```{.haskell .literate}
-- | Between 0.0 and 1.0
newtype Probability = Prob { unProb :: Double }
  deriving (Num, Fractional, Real, Floating, Ord, Eq,
            CoArbitrary, Show)

isValidProbability :: Probability -> Bool
isValidProbability p = p>= 0 && p<=1.0

instance Arbitrary Probability where
  arbitrary = sized $ \aSize -> do
      let precision = fromIntegral (aSize `max` startingPrecision)
      denominator <- choose (1, precision  )
      numerator   <- choose (0, denominator)
      pure         $ Prob (fromRational (numerator % denominator))
    where
      startingPrecision = 5
  shrink (Prob x) = filter isValidProbability (Prob <$> shrink x)
```
