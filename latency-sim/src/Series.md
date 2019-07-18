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
    mainfont: "DejaVu Serif"
    sansfont: Arial
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
module Series where

import GHC.Exts(IsList(..))
import Data.Semigroup

import Delay
```

# Appendix: Power series representing distributions

<a name="series">Here</a>
we follow [@PowerSeries] exposition of power series,
but use use finite series and shortcut evaluation:
```{.haskell .literate}
-- | Series contain the same information as lists.
--   We can also define lexicographic ordering on them.
newtype Series a = Series { unSeries :: [a] }
  deriving (Eq, Ord, Show)

-- | Cumulative sums computes sums of 1..n-th term of the series
cumsum :: Num a => Series a -> Series a
cumsum = Series . tail . scanl (+) 0 . unSeries

-- | Series of discrete differences (assuming there was fake zero before the series,
--   to preserve information about first term.)
diff :: Num a => Series a -> Series a
diff (Series []) = Series []
diff (Series s ) = Series $ head s : zipWith (-) (tail s) s

-- | Subtractive remainders computes running remainders to 1.0 after summing up
--   all terms of the series up to a given position.
--   This is convenience in case we decide to switch to continued fractions.
subrem :: Num a => Series a -> Series a
subrem = Series . scanl (-) 1 . unSeries

-- | Cut series at a given index.
cut :: Delay -> Series a -> Series a
cut (Delay t) (Series s) = Series (take t s)

instance Functor Series where
  fmap f (Series a) = Series (fmap f a)

instance Foldable Series where
  foldr f e (Series s) = foldr f e s

instance Semigroup (Series a) where
  Series a <> Series b = Series (a <> b)

instance IsList (Series a) where
  type Item (Series a) = a
  fromList          = Series
  toList (Series s) = s

-- | Scalar multiplication
infixl 7 .* -- same precedence as *
(.*):: Num a => a -> Series a -> Series a -- type declaration for .*
c .* (Series (f:fs)) = Series (c*f : unSeries ( c.* Series fs)) -- definition of .*
_ .* (Series []    ) = Series []

-- | Convolution in style of McIlroy
convolve :: Num a => Series a -> Series a -> Series a
Series (f:fs) `convolve` gg@(Series (g:gs)) = Series
  (f*g : unSeries (f .* Series gs + (Series fs `convolve` gg)))
Series []     `convolve` _                  = Series []
_             `convolve` Series []          = Series []

-- | Elementwise multiplication, assuming missing terms are zero.
(.*.) :: Num a => Series a -> Series a -> Series a
Series a .*. Series b = Series (zipWith (*) a b)

instance Num a => Num (Series a) where
  Series a + Series b = Series (go a b)
    where
      go    []     ys  = ys
      go    xs     []  = xs
      go (x:xs) (y:ys) = (x+y):go xs ys
  (*) = convolve
  abs = fmap abs
  signum = fmap signum
  fromInteger = error "Do not use fromInteger on Series!!!"
  negate = fmap negate

```
