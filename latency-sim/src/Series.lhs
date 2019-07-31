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
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}
module Series where

import GHC.Exts(IsList(..))
import Data.Foldable
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
newtype Series a = Series { unSeries :: [a] }
  deriving (Eq, Show, Read)
```
Generating function of :
$$ F(t)=f_0*t^0+f_1*t^1+f_2*t^2+...+f_n*t^n $$
is represented by the Haskell data structure:
```{.haskell .literate .ignore}
f_t = Series [a0, a1, .., an]
```

```{.haskell .literate}
-- | Cumulative sums computes sums of 1..n-th term of the series
cumsum :: Num a => Series a -> Series a
cumsum = Series
       . tail -- drop uninformative zero at the beginning of result
       . scanl (+) 0
       . unSeries
```
Differential encoding is lossless
correspondent of discrete differences, but with
first coefficient being copied.
(Just like there was a zero before each series,
 so that we always preserve information about the first term.)
This is _backward antidifference_ as defined by @wiki:antidifference.

That makes it an inverse of `cumsum`.
It is _backward finite difference operator_, as defined by @wiki:backwardFiniteDifference .

```{.haskell .literate}
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
(.*):: Num a => a -> Series a -> Series a
c .* (Series (f:fs)) = Series (c*f : unSeries ( c.* Series fs))
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
$$F(t)*G(t)=Σ_{0}^t x^t*(f_0*g_t+f_1*g_{t-1}+...+g_0*f_t) $$


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
```
Since we use finite series, we need to extend their length
when operation is done on series of different length.

Here we use extension by a given element `e`, which is 0 for normal series,
or 1 for complement series.
```{.haskell .literate}
-- | Extend both series to the same length with placeholder zeros.
--   Needed for safe use of complement-based operations.
extendToSameLength e (Series a, Series b) = (Series resultA, Series resultB)
  where
    (resultA, resultB) = go a b
    go  []       []  = (    [] ,    [] )
    go (b:bs) (c:cs) = (  b:bs',  c:cs')
      where
        ~(bs', cs') = go bs cs
    go (b:bs)    []  = (  b:bs, e  :cs')
      where
        ~(bs', cs') = go bs []
    go    []  (c:cs) = (e  :bs',  c:cs )
      where
        ~(bs', _  ) = go [] cs
```
In a rare case (CDFs) we might also prolong by the length of the last entry:
```
-- | Extend both series to the same length with placeholder of last element.
extendToSameLength' (Series a, Series b) = (Series resultA, Series resultB)
  where
    (resultA, resultB) = go a b
    go  []       []  = (    [] ,    [] )
    go  [b]     [c]  = (   [b] ,   [c] )
    go (b:bs) (c:cs) = (  b:bs',  c:cs')
      where
        ~(bs', cs') = go bs cs
    go (b:bs)   [c]  = (  b:bs,   c:cs')
      where
        ~(bs', cs') = go bs [c]
    go   [b]  (c:cs) = (b  :bs',  c:cs )
      where
        ~(bs', _  ) = go [b] cs
```
_Note: if we aim for more elegant presentation, we might come back to infinite
series and instead choose length of approximation when computing them._

Now we can present an instance of number class for `Series`:
```{.haskell .literate}
instance Num a => Num (Series a) where
  Series a + Series b = Series (go a b)
    where
      go    []     ys  = ys
      go    xs     []  = xs
      go (x:xs) (y:ys) = (x+y):go xs ys
  (*)         = convolve
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = seriesFromInteger
  negate      = fmap negate
```
Note that we do not know yet how to define `fromInteger` function.
Certainly we would like to define null and unit (neutral element) of convolution,
but it is not clear what to do about the others:
```{.haskell .literate}
seriesFromInteger     0 = [0] -- null
seriesFromInteger     1 = [1] -- unit
seriesFromInteger other = error
                        $ "Do not use fromInteger "
                       <> show other <> " to get Series!"
```
We may be using `Series` of floating point values that are inherently approximate.

In this case, we should not ever use equality, but rather a similarity metric
that we can generate from the similar metric on values:
```{.haskell .literate}
instance Real           a
      => Metric (Series a) where
  a `distance` b = sqrt $ realToFrac $ sum $ fmap square $ a-b
  similarityThreshold = 0.001

square x = x*x
```
Note that generous similarity threshold of `0.001` is due to limited
number of simulations we run when checking distributions (10k by default).

`Metric` class is a pretty typical for similarity measurement:
```{.haskell .literate}
class Metric a where
  distance :: a -> a -> Double
  similarityThreshold :: Double

infix 3 ~~

(~~) :: Metric t => t -> t -> Bool
(~~) (a::t) (b::t) = distance a b < similarityThreshold @t
```

```{.haskell .literate .hidden}
spec = quickCheckAll
```
