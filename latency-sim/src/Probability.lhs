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
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}
module Probability(Probability(..)
                  ,isValidProbability) where

import Data.Data
import Data.Typeable
import GHC.Generics

import Data.Ratio((%), Ratio)

```

# Appendix: Probability data type

<a name="probability">Probability</a>
is defined here for reference:
```{.haskell .literate}
-- | Between 0.0 and 1.0
newtype Probability = Prob { unProb :: Ratio Integer }
  deriving (Num, Fractional, Real, Ord, Eq,
            Read, Show, Data, Typeable, Generic)

isValidProbability :: Probability -> Bool
isValidProbability p = p>= 0 && p<=1.0
```
