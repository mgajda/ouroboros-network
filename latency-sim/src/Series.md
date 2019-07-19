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
module Series where

import GHC.Exts(IsList(..))
import Data.Semigroup

import Delay

import Test.QuickCheck
import Test.QuickCheck.All
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
```
Generating function of :
$$ F(t)=f_0*t^0+f_1*t^1+f_2*t^2+...+f_n*t^n $$
is represented by the Haskell data structure:
```{.literate}
f_t = Series [a0, a1, ..., an]
```

```{.haskell .literate}
-- | Cumulative sums computes sums of 1..n-th term of the series
cumsum :: Num a => Series a -> Series a
cumsum = Series
       . tail -- drop uninformative zero at the beginning of result
       . scanl (+) 0
       . unSeries

-- | Differential encoding is lossless
--   correspondent of discrete differences, but with
--   first coefficient being copied.
--   (Like there was fake zero before the series,
--    to preserve information about first term.)
--
--   That makes it an inverse of `cumsum`.
diffEnc :: Num a => Series a -> Series a
diffEnc (Series []) = Series []
diffEnc (Series s ) = Series $ head s : zipWith (-) (tail s) s
```
This serves to get cumulants:
```{.haskell .literate .ignored}
test_simpleCumsum  = cumsum [1,1,1] == ([1,2,3] :: Series Int)
test_simpleCumsum2 = cumsum [1,2,3] == ([1,3,6] :: Series Int)
test_cumsumIsLeftAdjointOfDiffEnc  x = cumsum  (diffEnc x) == x
test_cumsumIsRightAdjointOfDiffEnc x = diffEnc (cumsum  x) == x
```
So that `diffEnc` of CDF will get PDF,
and `cumsum` of PDF will get CDF.
```{.haskell .hidden}
-- | Subtractive remainders computes running remainders to 1.0 after summing up
--   all terms of the series up to a given position.
--   This is convenience in case we decide to switch to continued fractions.
subrem :: Num a => Series a -> Series a
subrem = Series . scanl (-) 1 . unSeries
```
```{.haskell .literate}
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
```
$$ F(t)=f_0*t^0+f_1*t^1+f_2*t^2+...+f_n*t^n $$
Convolution:
$$F(t)*G(t)=\Sigma_{0}^t x^t*(f_0*g_t+f_1*g_{t-1}+...+g_0*f_t) $$
Wikipedia's definition:
$$(f * g)(t) \triangleq\ \int_{-\infty}^\infty f(\tau) g(t - \tau) \, d\tau.$$
Distribution is from $0$ to $+\infty$:
Wikipedia's definition:

1. First we fix the boundaries of integration: $$(f * g)(t) \triangleq\ \int_{0}^\infty f(\tau) g(t - \tau) \, d\tau.$$
(Assuming $f(t)=g(t)=0$ when $t<0$.)

2. Now we change to discrete form:

$$(f * g)(t) \triangleq\ \Sigma_{0}^\infty f(\tau) g(t - \tau)$$

3. Now we notice that we approximate functions up to term $n$:
$$(f * g)(t) \triangleq\ \Sigma_{0}^{n} f_{\tau} g_{t - \tau}.$$

Resulting in convolution:
$$F(t)*G(t)=\Sigma_{0}^t x^t*(f_0*g_t+f_1*g_{t-1}+...+g_0*f_t) $$


```{.haskell .literate}
-- | Convolution in style of McIlroy
convolve :: Num a => Series a -> Series a -> Series a
Series (f:fs) `convolve` gg@(Series (g:gs)) =
  Series
    (f*g :
      unSeries (f .* Series gs +
               (Series fs `convolve` gg)))
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

```{.haskell .literate .hidden}
spec = quickCheckAll
```
