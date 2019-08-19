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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}
module KOutOfN where

import GHC.TypeLits(KnownNat)
import Data.Function(on)

import Complement
import Latency
import Series
import ShowUtils
import SMatrix
import Probability
```
# Histogramming

To provide histograms of average number of nodes reached by the broadcast,
we need to define additional operations:
* sum of mutually exclusive events
* K-out-of-N synchronization

## Sum of mutually exclusive events

First we need to define a precise sum of two events that are mutually exclusive $⊕$.
That is different from `firstToFinish` which assumes that they are mutually independent.
```{.haskell .literate}
infixl 5 `exSum` -- like addition
class ExclusiveSum a where
  exAdd :: a -> a -> a

exSum :: ExclusiveSum a => [a] -> a
exSum  = foldr1 exAdd

instance ExclusiveSum         Probability  where
  exAdd = (+)

-- NOTE: Here we assume expansion by unit of exclusive sum:
instance ExclusiveSum a => ExclusiveSum (Series a) where
  Series a `exAdd` Series b = Series
                            $ zipWithExpanding exAdd a b

instance ExclusiveSum LatencyDistribution where
  LatencyDistribution a `exAdd` LatencyDistribution b =
    LatencyDistribution (a + b)
```

## *K-out-of-N synchronization* of series of events $a_k$.

For histogramming a fraction of events that
have been delivered within given time, we use generalization of recursive
formula $\binom{n}{k}$.

$$ F_{\binom{a_n}{k}}(t) = a_n ∧ F_{\binom{a_{n-1}}{k-1}}(t)
                         ⊕ (\overline{a_n} ∧ F_{\binom{a_{n-1}}{k}}(t)) $$

Here $a_{n-1}$ is a finite series without its last term (ending at index $n-1$).

It is more convenient to treat $F_{\binom{a_n}{k}}(t)$ as a `Series` with
indices ranging over $k$: $F_{\binom{a_n}{...}}(t)$.
Then we see the following equation:
$$F_{\binom{a_{n}}{...}} = [\overline{a_n}, a_n] ⊛ F_{\binom{a_{n-1}}{...}} $$
Where:

* $⊛$ is convolution
* $[\overline{a_n}, a_n]$ is two element series having complement of $a_n$ as a first
term, and $a_n$ as a second term.

We implement it as a series `k_of_n` with parameter given as series $a_n$,
and indices ranging over $k$:
```{.haskell .literate}
kOutOfN :: (TimeToCompletion a
           ,ExclusiveSum     a
           ,Complement       a
           ) => Series       a
             -> Series       a
kOutOfN (Series []    ) = error "kOutOfN of empty series"
kOutOfN (Series [x]   ) = [complement x, x]
kOutOfN (Series (x:xs)) = [x] `convolution` Series xs
  where
    convolution = convolve_ exAdd lastToFinish `on` kOutOfN
```
### Fraction of reached nodes
Now, for a connection matrix $A$, each row corresponds to a vector
of latency distribution for individual nodes. Naturally source node is
indicated as a _unit_ on a diagonal.
Now we can use `kOutOfN` to transform the series corresponding to a single row
vector into a distribution of latencies for reaching _k-out-of-n_ nodes.
Note that this new series will have _indices_ corresponding to
*number of nodes reached* instead of node indices:
```{.haskell .literate}
nodesReached :: Series LatencyDistribution -> Series LatencyDistribution
nodesReached  = kOutOfN
```
For this we need to define `complement` for `LatencyDistribution`:
```{.haskell .literate}
instance Complement LatencyDistribution where
  complement (LatencyDistribution s) = LatencyDistribution
                                     ( complement <$> s )
```

## Averaging broadcast from different nodes

Given that we have a connection matrix $A$ of broadcast iterated $n$ times,
we might want histogram of distribution of a fraction of nodes
reached for a random selection of source node.

We can perform this averaging with exclusive sum operator, pointwise division of
elements by the number of distributions summed:

```{.haskell .literate}
averageKOutOfN  :: KnownNat n
                => SMatrix n LatencyDistribution
                -> Series LatencyDistribution
averageKOutOfN m = average (nodesReached . Series <$> rows m)
  where
    average :: [Series LatencyDistribution] -> Series LatencyDistribution
    average aList =  scaleLD
                 <$> exSum aList
      where
        len :: Probability
        len = Prob $ 1/fromIntegral (length aList)
        scaleLD :: LatencyDistribution -> LatencyDistribution
        scaleLD (LatencyDistribution s) = LatencyDistribution
                                         ((*len) <$> s)
```
NOTE:
  _We need to use weighted averaging, if we bias source node selection._

```{.haskell .hidden}
saveSurface :: FilePath -> Series LatencyDistribution -> IO ()
saveSurface fp curves = writeFile fp
                      $ joinLines content ""
  where
    content :: [ShowS]
    content = zipWith (showCurve showProb) [0..] (unpack <$> unSeries curves)
    unpack :: LatencyDistribution -> [Probability]
    unpack (LatencyDistribution (Series ls)) = ls

showCurve :: (a -> ShowS) -> Int -> [a] -> ShowS
showCurve f i = joinLines
              . zipWith (showPoint f i) [0..]

showProb :: Probability -> ShowS
showProb (Prob x) = (shows :: Double -> ShowS) (realToFrac x)

showPoint :: (a -> ShowS) -> Int -> Int -> a -> ShowS
showPoint f i j v = shows i
                  . (' ':)
                  . shows j
                  . (' ':)
                  . f v
```
