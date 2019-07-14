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
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}
module Latency where

import GHC.Exts(IsList(..))
import Data.Function(on)
import Data.Time
import Data.Ratio((%))
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen(vectorOf)
import Test.QuickCheck.Modifiers

```

# Introduction

In order to accurately simulate capacity-insensitive network miniprotocols
[@cardanoNetworkRequirements],
we formally define network latency distribution as improper CDF
(cumulative distribution function) of arrived messages over time.
We call it improper CDF, because it does not end at 100%,
since some messages can be lost.

Starting with description of its apparent properties, we identify
their mathematical definitions, and ultimately arrive at algebra of ΔQ
with basic operations that correspond to abstract interpretations
of network miniprotocols[@ProgramAnalysis].

This allows us to use objects from single algebraic body to describe
behaviour of entire protocols as improper CDFs.


## Related work
Our goal here is to establish realm of reasonable performance metrics for
Peer Discovery and similar miniprotocols of Cardano settlement layer
[@PeerDiscovery].

Then we discuss expansion of the concept to get most sensitive metrics
of protocol and network robustness[@NetworkRobustness].
However instead of heuristic measure like effective graph resistance
[@EffectiveGraphResistance], we use logically justified measure derived
from the actual behaviour of the network.

This is similar to *network calculus* but uses simpler methods
and uses more logical description with improper latency distribution functions,
instead of *max-plus* and *min-plus* algebras.
^[We describe how it generalizes these *max-plus* and *min-plus* algebras later.]

This approach allows us to use *abstract interpretation*[@ProgramAnalysis]
of computer program to get its latency distribution, or a single execution
to approximate latency distribution assuming the same loss profile of packets.

# Curious properties of latency distributions

## Introducing improper CDF

To define our key object, lets imagine a single network connection.
In this document, we ignore capacity-related issues, so $∆Q(t)$ is simply
*improper cumulative distribution function* of event arriving at some point
of time:

<center>

![Completion rate against deadline](completion-rate.png "Completion rate against deadline"){.center} \

![Latency distribution](latency-distribution.png "Latency distribution"){.center}\

</center>

For the sake of practicality, we also mark a *deadline* as the last possible
moment when we still care about messages. (Afterwards, we drop them, just like
TCP timeout works.)

Note that (say) only $0.99$ of messages arrive
at all within desired time $t$, and we silently drop those that arrive later.

For each distribution, we will define *deadline* formally
as $d(t)=\text{maxarg}_{t}(ΔQ(t))$ or such $t$ for which our improper CDF reaches
maximum value.
We also define *ultimate arrival probability* formally as $a_{u}(ΔQ)=\max(ΔQ)$.
Our improper CDFs are assumed to be always defined within finite subrange
of delays, starting at $0$. Ultimate arrival probability allows us
to compare attenuation between two links.

In the following we define domain of *arrival probabilities* as
$\mathcal{A}\in[0,1.0]$, which is probability.

We also define domain of *time* or *delays* as $\mathcal{T}$.
We also call a domain of $ΔQ$ functions as
$\mathcal{Q}=(\mathcal{T}\rightarrow{}\mathcal{A})$.

Below is Haskell specification of this datatype:
```{.haskell .literate}

newtype Probability = Prob { unProb :: Double } -- between 0.0 and 1.0
  deriving (Num, Fractional, Real, Floating, Ord, Eq,
            CoArbitrary, Show)
newtype Delay       = Delay Int
  deriving (Num, Ord, Eq, Enum, Bounded, CoArbitrary, Show)

instance Arbitrary Probability where
  arbitrary = sized $ \aSize -> do
      let precision = fromIntegral (aSize `max` minPrecision)
      denominator <- choose (1, precision  )
      numerator   <- choose (0, denominator)
      pure         $ Prob (fromRational (numerator % denominator))
    where
      minPrecision = 10
  shrink (Prob x) = filter isValidProbability (Prob <$> shrink x)

isValidProbability :: Probability -> Bool
isValidProbability p = p>= 0 && p<=1.0

newtype LatencyDistribution =
  LatencyDistribution {
    -- | Monotonically growing like any CDF. May not reach 1.
    prob :: Series Probability
  }

-- | Lexicographic ordering to get the equality
instance Eq LatencyDistribution where
  xs == ys = (lexCompare `on` (unSeries . prob)) xs ys == EQ
    where
      lexCompare :: [Probability] -> [Probability] -> Ordering
      lexCompare  xs     []    = if all (==0.0) xs
                                 then EQ
                                 else LT
      lexCompare  []     xs    = invertComparison $ lexCompare xs []
      lexCompare (x:xs) (y:ys) = case compare x y of
                                EQ    -> lexCompare xs ys
                                other -> other
      invertComparison LT = GT
      invertComparison GT = LT
      invertComparison EQ = EQ

instance IsList LatencyDistribution where
  type Item LatencyDistribution = Probability
  fromList = LatencyDistribution . Series
  toList   = unSeries . prob

instance Show LatencyDistribution where
  showsPrec _ ld s = "LatencyDistribution "++ showsPrec 0 (fmap unProb $ unSeries $ prob ld) s

-- | Validity criteria
isValidLD :: LatencyDistribution -> Bool
isValidLD []            = False
isValidLD [x]           = True -- any distribution with a single-element domain is valid (even one that delivers nothing)
isValidLD (last . unSeries . prob -> 0.0) = False -- any distribution with more than one element and last element of zero is invalid (to prevent redundant representations.)
isValidLD (prob -> probs) = (sum probs) <= 1.0 -- sum of probabilities shall never exceed 0.0
                         && all isValidProbability probs -- each value must be valid value for probability

canonicalizeLD = assureAtLeastOneElement . dropTrailingZeros
  where
    assureAtLeastOneElement []    = [0.0]
    assureAtLeastOneElement other = other
    dropTrailingZeros             = reverse . dropWhile (==0.0) . reverse

cdf :: LatencyDistribution -> Series Probability
cdf = cumsum . prob
start = 0
```

For ease of implementation, we express each function as a series of values
for discrete delays. First value is for *no delay*.
We define $start \in{}\mathcal{T}$ as smallest `Delay` (no delay).

## Intuitive properties of ΔQ

1. We can define few *linear* operators on ΔQ (for exact definition, see next section):

    A. Stretching in time -- ignored in here.

    B. Delaying by $t$ -- composition with $\text{wait}$:
$$wait(t)=f(t)= \begin{cases} 0 & \text{for } t<t_d \\ 1.0 & \text{for } t=t_d \end{cases}$$

    C. Scaling arrival probability -- in other words, $\text{attenuation}$.

2. We distinguish special distribution that represents *null delay* or *neutral element* of sequential composition, where we pass every message with no delay: $$attenuated(1)=wait(0)=1_{\mathcal{Q}}$$

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

## Operations on ΔQ

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
rd1 `afterLD` rd2 = LatencyDistribution {
                  --  deadline = deadline   rd1      +     deadline   rd2
                    prob     = prob rd1 `convolve` prob rd2
                  }
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

Here is the Haskell code for naive definition of these two operations:
We can also introduce alternative of two completion distributions.
It corresponds to a an event that is earliest possible conclusion of one
of two alternative events:
```{.haskell}
rd1 `firstToFinishLD` rd2 = LatencyDistribution {
  -- deadline = deadline   rd1 `max` deadline   rd2
     prob     = prob rd1  +  prob rd2
              - prob rd1 .*. prob rd2
  }
(∨) :: TimeToCompletion ttc => ttc -> ttc -> ttc
(∨) = firstToFinish
```
Now let's define neutral elements of both operations above:
```{.haskell}
attenuated a = LatencyDistribution {
    prob = Series [a]
  }
allLostLD = attenuated 0.0
noDelayLD = attenuated 1.0
```
Here:
- `allLost` indicates that no message arrives ever through this connection
- `noDelay` indicates that the all messages always arrive without delay

3. Conjunction of two different actions simultaneously completed in parallel, and waits
until they both are:
```{.haskell}
-- For: deadline = deadline   rd1 `max` deadline   rd2
rd1 `lastToFinishLD` rd2 = LatencyDistribution {
    prob = prob rd1 .*. cdf2 + prob rd2 .*. cdf1 - prob rd1 .*. prob rd2
  }
  where
    cdf1 = cumsum $ prob rd1
    cdf2 = cumsum $ prob rd2
(∧) :: TimeToCompletion ttc => ttc -> ttc -> ttc
(∧)=lastToFinish
```

Now we can make an abstract interpretation of protocol code to derive
corresponding improper CDF of message arrival.

4. Failover $A<t>B$ when action is attempted for a fixed period of time $t$,
   and if it does not complete in this time, the other action is attempted:
```haskell
failover deadline rdTry rdCatch =
    LatencyDistribution {
      --  deadline = deadline rdTry
      --           + deadline rdCatch
        prob     = initial <> fmap (remainder*) (prob rdCatch)
    }
  where
    initial :: Series Probability
    initial = cut deadline $ prob rdTry
    -- | Subtractive remainder of the accepted part of time-to-complete:
    remainder :: Probability
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

6. Predictive Network Solutions (Neil and Peter) proposed using operator
    $A⇆_p B$ for probabilistic
    choice between scenarios $A$ with probability $p$, and $B$ with probability
    $1-p$. This is not necessary, as long as we assume that the only
    way to get non-determinism is due to latency, and our miniprotocols
    involve only deterministic computation, and *unique agency property*
    described by Marcin.

## Estimates
Note that we can define derived estimates of
`LatencyDistribution`
that somewhat approximate it:
```haskell
newtype Latest = Latest { unLatest :: Delay }
  deriving (Eq, Ord, Show)
latest :: LatencyDistribution -> Latest
latest  = Latest . Delay . (-1+) . length . unSeries . prob
onLatest = liftBinOp unLatest Latest

newtype Earliest = Earliest { unEarliest :: Delay }
  deriving (Eq, Ord, Show)
earliest :: LatencyDistribution -> Earliest
earliest [x]                             = Earliest 0
earliest (last . unSeries . prob -> 0.0) = error "LatencyDistribution should always end with non-zero value"
earliest  other                          = Earliest . Delay . (max 0) . length . takeWhile (0==) . unSeries . prob $ other

-- TODO: place for a lens?
onEarliest = liftBinOp unEarliest Earliest

instance TimeToCompletion Earliest where
  firstToFinish = onEarliest min
  lastToFinish  = onEarliest max
  after         = onEarliest (+)
  noDelay       = Earliest 0

instance TimeToCompletion Latest where
  firstToFinish = onLatest min
  lastToFinish  = onLatest max
  after         = onLatest (+)
  noDelay       = Latest 0

```
These estimates have the property that we can easily compute
the same operations on estimates, without really computing
the full `LatencyDistribution`.

```{.haskell}
class TimeToCompletion ttc where
  firstToFinish :: ttc -> ttc -> ttc
  lastToFinish  :: ttc -> ttc -> ttc
  after         :: ttc -> ttc -> ttc
  noDelay       :: ttc
  {-# MINIMAL firstToFinish, lastToFinish, after, noDelay #-}
  -- | Add explicit case/if to make correct estimate
  --   single pass instead of trace

instance TimeToCompletion LatencyDistribution where
  firstToFinish = firstToFinishLD
  lastToFinish  = lastToFinishLD
  after         = afterLD
  noDelay       = noDelayLD

-- | Specify the functor with respect to TTC operations
verifyTTCFunctor compatible extract a b =
     (         (a `firstToFinish`         b) `compatible`
      (extract  a `firstToFinish` extract b)) &&
     (         (a `lastToFinish`          b) `compatible`
      (extract  a `lastToFinish`  extract b)) &&
     (         (a `after`                 b) `compatible`
      (extract  a `after`         extract b)) &&
                   noDelay                   `compatible` noDelay

```

# Representing networks

Adjacency matrix is classic representation of network graph,
where $i$-th row corresponds to outbound edges of node $i$,
and $j$-th column corresponds to inbound edges of node $j$.
So $A_{i,j}$ is $1$ when edge is connected, and $0$ if edge is not connected.

Generalizing this to ΔQ-matrices, we get: any non-zero value describing
quality of valid connection between nodes from $i$ to $j$,
and zero value `allLost` corresponding to nodes that are not directly connected.

We can generalize simple way of checking that graph is strongly connected:


$R_n(A)=1+A+A^2+...+A^n$

Our key metric would be diffusion or reachability time of the network $∆R(t)$, which is conditioned
by quality of connection curves $∆Q(t)$ and the structure network graph.

Later in this section, we discuss on how $∆R(t)$ encompasses other plausible
performance conditions on the network.


### Reachability of network broadcast or ∆R(t)

Reachability curve $∆R(t)$ or _diffusion time_ is plotted as distribution
of event where all nodes are reached by broadcast from committee node,
against time.
We want to sum the curve for all possible core nodes, by picking a random
core node.

Area under the curve would tell us the overall quality of the network.
When curve reaches 100% rate, then we have a strongly connected network,
which should be eventually always the case after necessary
reconfigurations.

_Note that when running experiments on multiple networks, we will need to indicate when we show average
statistics for multiple networks, and when we show a statistic for a single network._

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
```{.haskell}
scaleProbability a = after $ attenuated a
scaleDelay  :: Delay -> LatencyDistribution -> LatencyDistribution
scaleDelay       t = after $ delay      t

delay :: Delay -> LatencyDistribution
delay n = LatencyDistribution
        $ Series
        $ [0.0 | _ <- [(0::Delay)..n-1]] <> [1.0]
```

### Description of network connectivity graph in terms of ∆Q

Traditional way of describing graphs is by adjacency matrix,
where 0 means there is no edge, and 1 means that there is active edge.

We may generalize it to unreliable network connections described above,
by using $ΔQ$ instead of binary.

So for each value diagonal, the network connection matrix $A$ will be `noDelay`,
and $A_{i j}$ will represent the connection quality for messages sent from
node $i$ to node $j$.

That allows us to generalize typical graph algorithms executed to
algorithms executed on network matrices:

1. If series $R_n(A)=1+A+A^2+...+A^n$ converges to matrix of non-zero
(non-`allLost`) values *in all cells* in a finite number of steps,
we consider graph to be *strongly connected* [@GeneralMethodOfShortestPaths].
Matrix multiplication follows uses $(\mathbf{;},∨)$-modulus.
(So sequential composition in place of multiplication,
  and alternative selection in place of addition.)

This series is called $A^{*}$ and that makes latency distribution class
of metrics *transitive closure semirings* [@TransitiveClosureSemirings].

Note that making latency distributions a proper semiring requires appropriate
definition of $\delta{}Q$ [@NetworkReliabilityNotSemirings]

Also note that this series converges to $ΔQ$ on
a single shortest path between each two nodes.
That means that we may call this matrix $R_{min}(t)$,
or optimal diffusion matrix.
```haskell
-- | This computes optimal connections on $\deltaQ{}$ matrix.
--   TODO: Check that it works both for boolean matrices, and $\deltaQ{}$.
optimalConnections a = fix step a
  where
    step r = r+a |*| r

fix step r = if r `almostEqual` r'
           then          r
           else fix step r'
  where
    r' = step r
    almostEqual :: Num a => a -> a -> Bool
    almostEqual a1 a2 = frob (a1-a2)<=epsilon
      where
        epsilon=0.001
        -- | Any reasonable measure of matrix divergence
        --divergence = maximum . fmap abs . fromList
```

We will use this to define *path with shortest ΔQ*.
It corresponds to the situation where all nodes broadcast value from
any starting point $i$ for the duration of $n$ retransmissions.
^[That we do not reduce loss over remainder yet?]

2. Considering two nodes we may consider delay introduced by
   retransmissions in naive miniprotocol:
   * we have two nodes *sender* and *receiver*
   * *sender* sends message once per period equal maximum network latency
     `deadline`
   * the message is *resent* if *receiver* fails to send back confirmation
     of receipt
   ...
   Assuming latency of the connection $l$, and timeout $t>d(l)$, we get simple
   solution:
   $$ \mu{}X.l\mathbf{;}l\mathbf{;}X $$

3. We need to consider further examples of how our metrics react
   to issues detected by typical graph algorithms.


### Performance goals

The *optimal network* performance can be reached for
an active connection network that contains shortest path
between each two nodes.
We aim for a performance within fixed factor of this
*optimal network*.

_Note that optimal network is almost never just
optimal spanning tree_
We also want to set a reasonable *stabilization time bounds*
before this performance goal is reached.

### Relation to other plausible metrics

One can imagine other key properties that network must satisfy:

* That absent permanent failures, network will reach full connectivity.
That corresponds to the situation where given ∆Q(t) iCDF ultimately reaches 100%
delivery probabiliity for some delay, ∆R(t) will also always reach 100%.
*Moreover ∆R(t) metric allows to put deadline for reaching full connectivity
in a convenient way.*
* That given a fixed limit on rate of nodes joining and leaving the network,
we also will have deadline on when ∆R(t) reaches a fixed delivery rate $R_{SLA}$
within deadline $t_{SLA}$.
* That given conditions on minimum average quality of connections $∆Q(t)$,
and fixed rate of adversary nodes $r_{adv}$ we can still guarantee networks
reaches reachability $R_{SLA}$.
* That there are conditions for which $∆R(t)$ always reaches almost optimal
reachability defined by given ratio $o\in{}(0.9,1.0)$, such that
$∆R(o*t) \ge max(∆R_{optimal}(t)/o)$.
In other words: there is a deadline $o$-times longer than time to reach optimal
reachability in an optimal network, we reach connectivity of no less than
$o$-times connectivity of the optimal network.

## What for capacity-limited networks?

Our environment will compute maximum flow of messages sent between
nodes at each point of time.
Our proofs can also use constant bound on the number of messages
sent between nodes at each step of the protocol.
That should assure that our capacity-insensitive approximations
stay valid for all networks that have significantly more capacity
than used by the algorithm.

# Schedule
## Milestones proposed

1. Possible metrics explored
    * mathematical characterization
    * implementation
    * algebraic property exploration
2. Comparison with other graph metrics
3. Properties of the metric
      * enumeration of desired or expected properties
      * derive algebra of the chosen metric
4. Implementation of the metric
      * datatype for $∆Q(t)$ and $∆R(t)$.
        and optimization of reduction of $∆Q(t)$
        computation.
      * computing $∆Q(t)$ and $∆R(t)$ curve for random network
      connections.
5. Comparison with other metrics used for graph and network research
      * visualizing one or multiple distributions
      * assertions about classical graph algorithms vs rates
      * computing metrics random networks

## Desired outcomes:
* mathematical theory and statistical tool for modelling tool latencies
  over high capacity networks
* statistical assessments of these policies under adverse scenarios

## Future plans:
* proof that our policies are almost-optimal after convergence
* expand the theory into capacity-limited networks
* apply it to reason about latency of other protocols within Cardano
* modelling a most adverse scenario [@ProgrammingSatan]


# Acknowledgments

Michał started writing, since the task seemed so easy to do within a month
or two.
Duncan made sure that discussion constructively proceeds
for reasonable amount of time,
and everybody follows. Marcin made sure that nobody breaks the family
glasses. Karl provided online source of temptations to delay release.

# Glossary

* $t∈\mathcal{T}$ - time since sending the message, or initiating a process
* $∆Q(t)$ - response rate of a single connection after time $t$
  (chance that message was received until time $t$)
* $∆R(t)$ - completion rate of broadcast to entire network (rate of nodes
  expected to have seen the result until time $t$)
* $\epsilon{}$ - rate of packets that are either dropped or arrive after
  latest reasonable deadline we chose

# Appendix: Missing definitions

```{.haskell}
(|*|) = undefined -- matrix multiplication
frob = undefined -- Frobenius metric
infixl 7 .* -- same precedence as *
-- | Scalar multiplication
(.*):: Num a => a -> Series a -> Series a -- type declaration for .*
c .* (Series (f:fs)) = Series (c*f : unSeries ( c.* Series fs)) -- definition of .*
c .* (Series []    ) = Series []
-- | Convolution from McIlroy
convolve :: Num a => Series a -> Series a -> Series a
(Series (f:fs)) `convolve` gg@(Series (g:gs)) =
  Series (f*g : unSeries (f .* Series gs + (Series fs `convolve` gg)))
(Series []    ) `convolve` _                  =
  Series []

elementwise f (Series a) (Series b) = Series (go f a b)
  where
    go f []        ys  = ( f  0.0) <$>        ys
    go f xs      []    = (`f` 0.0) <$>     xs
    go f (x:xs) (y:ys) = (x `f` y)  : go f xs ys

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
  fromInteger = undefined
  negate = fmap negate

-- | Lift binary operator to newtype.
liftBinOp unpack pack op a b = pack (unpack a `op` unpack b)

instance Arbitrary Delay where
  arbitrary = getNonNegative <$> arbitrary
  shrink (Delay d) = Delay <$> shrinkIntegral d

instance Arbitrary LatencyDistribution where
  arbitrary = sized $ \maxLen -> do
    actualLen <- choose (0, maxLen-1)
    ld <- LatencyDistribution . Series <$> case actualLen of
            0 -> (:[]) <$> arbitrary
            _ -> -- Last should be non-zero
                 (++) <$> vector actualLen
                      <*> ((:[]) . getPositive <$> arbitrary)
    if isValidLD ld
       then pure ld
       else arbitrary
  shrink (unSeries . prob -> ls) = LatencyDistribution <$> Series <$> recursivelyShrink ls
```

# Appendix: Power series

Here we follow [@PowerSeries]:
```haskell
-- | Put more code here...
newtype Series a = Series { unSeries :: [a] }
  deriving (Eq, Ord, Show)

-- | Cumulative sums computes sums of 1..n-th term of the series
cumsum = Series . scanl (+) 0 . unSeries
-- | Subtractive remainders computes running remainders to 1.0 after summing up
--   all terms of the series up to a given position.
subrem = Series . scanl (-) 1 . unSeries

--completion = undefined
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
```

# Bibliography
