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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
module Delay where

import GHC.Generics
import Data.Typeable
import Data.Data
import Test.QuickCheck

```
# Appendix: Discrete delay

<a name="delay">Discrete delays</a> are defined as:
```{.haskell .literate}
newtype Delay       = Delay { unDelay :: Int } 
  deriving (Num, Ord, Eq, Enum, Bounded, CoArbitrary, Show, Data, Typeable, Generic)

instance Arbitrary Delay where
  arbitrary = getNonNegative <$> arbitrary
  shrink (Delay d) = Delay <$> shrinkIntegral d

isValidDelay :: Delay -> Bool
isValidDelay (Delay d) = d>=0

start :: Delay
start = Delay 0
```
