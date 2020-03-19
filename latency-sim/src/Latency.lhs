---
input: markdown+tex_math_dollars+yaml_metadata_block+citations
output:
  pdf_document:
    keep_tex: true
    toc: true
    toc_depth: 2
    latex_engine: xelatex
    listings: true
bibliography:
  - Latency.bib
---

```{.haskell .hidden}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UnicodeSyntax              #-}
module Latency(
    Delay(..)
  , start
  , LatencyDistribution(..)
  , TimeToCompletion   (..)
  , preserved
  , canonicalizeLD
  , (~~)
  , scaleProbability
  , scaleDelay
  ) where

import GHC.Exts(IsList(..))

import Probability
import Delay
import Series
import Metric
import NullUnit

```
# Latency distributions

## Introducing improper CDF

To define our key object, lets imagine a single network connection.
In this document, we ignore capacity-related issues.
So $∆Q(t)$ is *improper cumulative distribution function* of event arriving at some point
of time:

<center>

![Completion rate against deadline](../doc/completion-rate.png "Completion rate against deadline"){ width=50% height=25%}

![Latency distribution](../doc/latency-distribution.png "Latency distribution"){ width=50% height=25% }

</center>

For the sake of practicality, we also mark a *deadline* as the last possible
moment when we still care about messages. (Afterwards, we drop them, just like
TCP timeout works.)

For example, when only $0.99$ of messages arrive
at all within desired time $t$, and we silently drop those that arrive later.

For each distribution, we will define *deadline* formally
as $d(t)=\text{maxarg}_{t}(ΔQ(t))$ or such $t$ for which our improper CDF reaches
maximum value.
We also define *ultimate arrival probability* formally as $a_{u}(ΔQ)=\max(ΔQ)$.
Our improper CDFs are assumed to be always defined within finite subrange
of delays, starting at $0$. Ultimate arrival probability
allows us
to compare attenuation between two links.

In the following we define domain of *arrival probabilities* as
$\mathcal{A}\in[0,1.0]$, which is [probability](#probability).

We also define domain of *time* or *delays* as $\mathcal{T}$.
We also call a domain of $ΔQ$ functions as
$\mathcal{Q}=(\mathcal{T}\rightarrow{}\mathcal{A})$.

Below is Haskell specification of this datatype:
```{.haskell .literate}
newtype LatencyDistribution a =
  LatencyDistribution { pdf :: Series a }
```
The representation above holds PDF (probability density function).
Its cumulative distribution function can be trivially computed with running sum:
```{.haskell .literate}
cdf :: Num                 a
    => LatencyDistribution a
    -> Series              a
cdf = cumsum . pdf
```

Since it is common use complement of CDF, we can have accessor for this one too:
```{.haskell .literate}
complementCDF :: Probability         a
              => LatencyDistribution a
              -> Series              a
complementCDF  = complement . cumsum . pdf
```

Since we use `canonicalizeLD` to make sure that every distribution is kept
in canonical form, we might also want to make constructors that create `LatencyDistribution`
from a series that represents PDF or CDF:
```{.haskell .literate}
fromPDF :: Probability         a
        => Series              a
        -> LatencyDistribution a
fromPDF = canonicalizeLD . LatencyDistribution
```

To create LatencyDistribution from CDF we need `diffEnc`
(_differential encoding_ or _backward finite difference operator_
 from [`Series` module](#series)):
```{.haskell .literate}
fromCDF :: Probability         a
        => Series              a
        -> LatencyDistribution a
fromCDF = canonicalizeLD . LatencyDistribution
        . diffEnc
```
Similar we can create `LatencyDistribution` from complement of CDF:
```{.haskell .literate}
fromComplementOfCDF :: Probability         a
                    => Series              a
                    -> LatencyDistribution a
fromComplementOfCDF  = canonicalizeLD
                     . LatencyDistribution
                     . diffEnc
                     . complement
```

For ease of implementation, we express each function as a series of values
for [discrete delays](#delay). First value is for *no delay*.
We define $start \in{}\mathcal{T}$ as smallest `Delay` (no delay).

```{.haskell .ignore}
start :: Delay
start  = Delay 0
```

## Intuitive properties of latency distributions

1. We can define few *linear* operators on ΔQ (for exact definition, see next section):

    A. Stretching in time -- ignored in here.

    B. Delaying by $t$ -- composition with $\text{wait}$:
$$wait(t)=f(t)= \begin{cases} 0 & \text{for } t<t_d \\ 1.0 & \text{for } t=t_d \end{cases}$$

    C. Scaling arrival probability -- in other words, $\text{attenuation}$.

2. We distinguish special distribution that represents *null delay* or *neutral element* of sequential composition, where we pass every message with no delay: $$preserved(1)=wait(0)=1_{\mathcal{Q}}$$

3. We can say that one $ΔQ$ no worse than the other,
   when it is improper CDF values never less than the other after making it fit a common
   domain:
$$
ΔQ_1≥_QΔQ_2 ≡∀t. X(ΔQ_1)(t)≥X(ΔQ_2)(t)
$$
Here assuming $X(ΔQ)$ defined:
$$
X(ΔQ)(t)≡\begin{cases}ΔQ(t)     & \text{for } t≤d(ΔQ)\\
                      ΔQ(d(ΔQ)) & \text{otherwise}
         \end{cases}
$$

## Basic operations on ΔQ

To model connections of multiple nodes, and network protocols we need two basic
operations:

1. Sequential composition $\mathbf{;}$: given $ΔQ_1(t)$ and $ΔQ_2(t)$ of
two different connections, we should be able to compute the latency function
for routing the message through pair of these connections
in parallel: $$ΔQ(t)=ΔQ_1(t)\mathbf{;}ΔQ_2(t)$$.
    * *associative*:
      $$ΔQ_1(t)\mathbf{;}[ΔQ_2(t)\mathbf{;}ΔQ_3(t)]=[ΔQ_1(t)\mathbf{;}ΔQ_2(t)]\mathbf{;}ΔQ_3(t)$$
    * *commutative*
      $$ΔQ_1(t)\mathbf{;}ΔQ_2(t)=ΔQ_2(t)\mathbf{;}ΔQ_1(t)$$
    * *neutral element* is $1_{\mathcal{Q}}$ or `noDelay`, so:
      $$ΔQ(t)\mathbf{;}1_{\mathcal{Q}}=1_{\mathcal{Q}}\mathbf{;}ΔQ(t)=ΔQ(t)$$

```{.haskell}
afterLD :: Probability a
        => LatencyDistribution a
        -> LatencyDistribution a
        -> LatencyDistribution a
rd1 `afterLD` rd2 = fromPDF
                  $ pdf rd1 `convolve` pdf rd2
```

2. Alternative selection ∨: given $ΔQ_1(t)$ and $ΔQ_2(t)$ of two different
connections, we should be able to compute the latency function
for routing the message through pair of these connections in parallel:
$ΔQ(t)=ΔQ_1(t)\mathbf{∨} ΔQ_2(t)$.
* *associative*:
  $$ΔQ_1(t)∨[ΔQ_2(t)∨ΔQ_3(t)]=[ΔQ_1(t)∨ΔQ_2(t)]∨ΔQ_3(t)$$
* *commutative*
  $$ΔQ_1(t)∨ΔQ_2(t)=ΔQ_2(t)∨ΔQ_1(t)$$
* *neutral element* is $0_{\mathcal{Q}}$ or `allLost`, so:
  $$ΔQ(t) ∨ 0_{\mathcal{Q}}=0_{\mathcal{Q}}∨ΔQ(t)=ΔQ(t)$$
* *null element* of `firstToFinish` is $1_Q$:
  $$ΔQ(t) ∨ 1_{\mathcal{Q}}=1_{\mathcal{Q}}∨ΔQ(t)=1_{\mathcal{Q}}$$
* *monotonically increasing*:
  $$ΔQ_1(t) ∨ ΔQ_2(t)≥ΔQ_1(t)$$


Here is the Haskell code for naive definition of these two operations:
We can also introduce alternative of two completion distributions.
It corresponds to a an event that is earliest possible conclusion of one
of two alternative events.

That can be easily expressed with improper cumulative distribution functions:
$$ \text{P}_{min(a,b)}(x \leq t)= 1-(1-P_a(x\leq t)) * (1-P_b(x\leq t))$$
That is, event $min(a,b)$ occured when $t<a$ or $t<b$, when:

* it **did not occur** (top complement: $1-...$), that:
    *  either $a$ did **not** occur $1-P_a(x≤t)$,
    *  and $b$ did **not** occur $1-P_b(x≤t)$:
```{.haskell .literate}
firstToFinishLD :: Probability a
                => LatencyDistribution a
                -> LatencyDistribution a
                -> LatencyDistribution a
rd1 `firstToFinishLD` rd2 =
    fromComplementOfCDF   $
      complementCDF rd1' .*.
      complementCDF rd2'
  where
    (rd1',
     rd2') = extendToSameLengthLD (rd1
                                  ,rd2)
```

Notes:

1. Since we model this with finite discrete series, we first need to extend them
to the same length.
2. Using the fact that `cumsum` is discrete correspondent of integration,
and `diffEnc` is its direct inverse (_backward finite difference_),
we can try to differentiate this to get PDF directly:
$$ \begin{array}{lcr} P_a(x≤t) & = & Σ_{x=0}^{t}P_a(x) \\
∇\left(Σ_0^{t}P_{a}(x)dx\right) & = & P_a(t) \\
\end{array}$$
In continuous domain, one can also differentiate both sides of the equation above
to check if we can save computations by computing PDF directly.

Unfortunately, that means that instead of 2x cumulative sum operations,
1x elementwise multiplication, and 1x differential encoding operation,
we still need to perform the same 2x cumulative sums, and 3x pointwise additions
and 3x pointwise multiplications, and two complements.

So we get an equation that is less obviously correct,
and more computationally expensive.

Code would look like:
```{.haskell .ignore}
rd1 `firstToFinishLD` rd2 = canonicalizeLD $
  LatencyDistribution {
     pdf      = rd1' .*. complementCDF rd2'
              + rd2' .*. complementCDF rd1'
              + rd1' .*. rd2'              }
  where
    (rd1', rd2') = extendToSameLengthLD (rd1, rd2)
    complement :: Series Probability -> Series Probability
    complement = fmap (1.0-)
```
_Note that `complement` above will be correct only if both lists are of the same
length._

In order to use this approach in here, we need to prove that `cumsum` and `diffEnc`
correspond to integration, and differentiation operators for discrete time domain.

Now let's define neutral elements of both operations above:
```{.haskell}
preserved :: Probability         a
          =>                     a
          -> LatencyDistribution a
preserved a = LatencyDistribution {
                pdf = Series [a]  }

allLostLD, noDelayLD :: Probability         a
                     => LatencyDistribution a
allLostLD = preserved 0.0
noDelayLD = preserved 1.0
```
Here:

- `allLost` indicates that no message arrives ever through this connection
- `noDelay` indicates that the all messages always arrive without delay

3. Conjunction of two different actions simultaneously completed in parallel, and waits
until they both are:
$$ \text{P}_{max(a,b)}(x \leq t)= P_a(x\leq t) * P_b(x\leq t)$$
```{.haskell}
lastToFinishLD :: Probability a
               => LatencyDistribution a
               -> LatencyDistribution a
               -> LatencyDistribution a
rd1 `lastToFinishLD` rd2 = fromCDF
                         $ cdf rd1' .*. cdf rd2'
  where
    (rd1', rd2') = extendToSameLengthLD (rd1, rd2)
```

(Attempt to differentiate these by parts also leads to more complex equation:
`rd1 .*. cumsum rd2 + rd2 .*. cumsum rd1`.)

Now we can make an abstract interpretation of protocol code to derive
corresponding improper CDF of message arrival.

It is also:

* commutative
* associative
* with neutral element of noDelay

4. Failover $A<t>B$ when action is attempted for a fixed period of time $t$,
   and if it does not complete in this time, the other action is attempted:
```{.haskell .literate}
failover deadline rdTry rdCatch = fromPDF $
    initial <> fmap (remainder*) (pdf rdCatch)
  where
    initial = cut deadline $ pdf rdTry
    remainder = 1 - sum initial
```

  Algebraic properties of this operator are clear:

   * Certain failure converts deadline into delay:
     $$\text{fail}<t>A=\text{wait}_t\mathbf{;}A$$
   * Failover to certain failure only cuts the latency curve:
     $$A<t>\text{fail}=\text{cut}_{t}A$$
   * Certain success ignores deadline:
     $$1_Q<t>A=1_Q \text{ when } t>0$$
   * Failover with no time left means discarding initial operation:
     $$A<0>B=B$$

   * When deadline is not shorter than maximum latency of left argument,
     it is similar to alternative, with extra delay before second argument:
     $$A<t>B=A ∨\text{wait}(t)B \text{ when } t>d(A)$$

5. Retransmission without rejection of previous messages: $A<t>>B$,
   when we have a little different algebraic properties with *uncut*
   left argument:
   $$A<t>B=A ∨ \text{wait}_t\mathbf{;}B$$
   $$A<0>B=A ∨ B$$
   $$A<t>\text{fail}=A$$
   $$\text{fail}<t>A=A$$

6. [Predictive Network Solutions (Neil and Peter)](http://www.pnsol.com) proposed using operator
    $A⇆_{p} B$ for probabilistic
    choice between scenarios $A$ with probability $p$, and $B$ with probability
    $1-p$. This is not necessary, as long as we assume that the only
    way to get non-determinism is due to latency, and our miniprotocols
    involve only deterministic computation, and *unique agency property*
    described by Marcin.

```{.hidden}
-- __
```

We can encapsulate basic operations with `TimeToCompletion` class, describing
interface that will be used both for `LatencyDistribution`s and their
approximations:
```{.haskell .literate}
class TimeToCompletion ttc where
  firstToFinish :: ttc -> ttc -> ttc
  lastToFinish  :: ttc -> ttc -> ttc
  after         :: ttc -> ttc -> ttc
  delay         :: Delay -> ttc
  allLost       :: ttc
  noDelay       :: ttc
  noDelay        = delay 0
  {-# MINIMAL firstToFinish, lastToFinish,
              after, delay, allLost #-}

infixr 7 `after`
infixr 5 `firstToFinish`
infixr 5 `lastToFinish`

instance Probability a
      => TimeToCompletion (LatencyDistribution a) where
  firstToFinish = firstToFinishLD
  lastToFinish  = lastToFinishLD
  after         = afterLD
  delay         = delayLD
  allLost       = allLostLD
  noDelay       = noDelayLD
```
### General treatment of completion distribution over time

Whether might aim for minimum delay distribution of message
over a given connection $∆Q(t)$ , minimum time of propagation
of the message over entire network ($∆R(t)$, reachability), we still
have a distribution of completion distribution over time with standard operations.

We will need a standard library for treating these to speed up our computations.

We can also define a mathematical ring of (probability, delay) pairs.

Note that `LatencyDistribution` is a modulus over ring R with `after` as multiplication,
and `whicheverIsFaster` as addition. Then `noDelay` is neutral element
of multiplication (unit or one), and `bottom` is neutral element of addition.
^[This field definition will be used for multiplication of connection matrices.]
Note that both of these binary operators give also rise to two
almost-scalar multiplication operators:
```{.haskell .literate}
scaleProbability :: Probability         a
                 =>                     a
                 -> LatencyDistribution a
                 -> LatencyDistribution a
scaleProbability a = after $ preserved a

scaleDelay  :: Probability         a
            => Delay
            -> LatencyDistribution a
            -> LatencyDistribution a
scaleDelay t = after $ delayLD t

delayLD :: Probability a
        => Delay
        -> LatencyDistribution a
delayLD n = LatencyDistribution
        $ Series
        $ [0.0 | _ <- [(0::Delay)..n-1]] <> [1.0]
```

```{.haskell .hidden}
-- Below are convenience functions for easy entry and display of latency distributions:

-- Allows usage of list syntax in place of distributions.
-- Requires:
-- `{-# LANGUAGE OverloadedLists #-}`
-- `import GHC.Exts(IsList(..))`
instance IsList (LatencyDistribution a) where
  type Item (LatencyDistribution a) = a
  fromList = LatencyDistribution . Series
  toList   = unSeries . pdf

instance Show                      a
      => Show (LatencyDistribution a) where
  showsPrec _ ld s = "LatencyDistribution "++ showsPrec 9 (fmap show $ unSeries $ pdf ld) s
```

To convert possibly improper `LatencyDistribution` into its canonical representation:
```{.haskell .literate}
canonicalizeLD :: Probability         a
               => LatencyDistribution a
               -> LatencyDistribution a
canonicalizeLD = LatencyDistribution
               . Series
               . assureAtLeastOneElement
               . dropTrailingZeros
               . cutWhenSumOverOne 0.0
               . unSeries
               . pdf
  where
    cutWhenSumOverOne aSum []           = []
    cutWhenSumOverOne aSum (x:xs)
                           | aSum+x>1.0 = [1.0-aSum]
    cutWhenSumOverOne aSum (x:xs)       =
        x:cutWhenSumOverOne (aSum+x) xs
    assureAtLeastOneElement []          = [0.0]
    assureAtLeastOneElement other       = other
    -- NOTE: this should be dropped
    -- when approximating by infinite series
    dropTrailingZeros                   = reverse
                    . dropWhile (==0.0) . reverse
```
To compare distributions represented by series of approximate values we need approximate equality:
```{.haskell .literate}
instance (Metric                a
         ,Num                   a
         ,Real                  a)
      => Metric
           (LatencyDistribution a) where
  LatencyDistribution l
    `distance`
      LatencyDistribution m =
        realToFrac $ sum $ fmap (^2) $ l-m
  similarityThreshold = 1/1000
```

Choosing `0.001` as similarity threshold (should depend on number of samples)

```{.haskell .literate}
instance Unit a
      => Unit (LatencyDistribution a) where
  unitE = LatencyDistribution (Series [unitE])

instance Null a
      => Null (LatencyDistribution a) where
  nullE = LatencyDistribution (Series [nullE])
```

```{.haskell .hidden}
extendToSameLengthLD :: Null a
                     => (LatencyDistribution a
                        ,LatencyDistribution a)
                     -> (LatencyDistribution a
                        ,LatencyDistribution a)
extendToSameLengthLD (LatencyDistribution ld1
                     ,LatencyDistribution ld2) =
    (LatencyDistribution ld1'
    ,LatencyDistribution ld2')
  where
    (ld1', ld2') = extendToSameLength nullE
                                     (ld1, ld2)
```
