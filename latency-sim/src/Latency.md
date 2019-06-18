---
author:
  - Michał J. Gajda
  - Peter     Thompson
  - Neil      Davies
  - Karl      Knutsson
  - Duncan    Coutts
  - Marcin    Szamotulski
title: Curious properties of latency distributions
abstract: |
  Network latency distributions, their algebra, and use examples.
date: June 12 2019, v1.8
input: markdown+tex_math_dollars+yaml_metadata_block
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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Latency where

import Data.Time
import Data.Semigroup

```

# Introduction

In order to accurately simulate capacity-insensitive network miniprotocols,
we formally define network latency distribution as improper CDF
(cumulative distribution function) of arrived messages over time.
We call it improper CDF, because it does not end at 100%,
since some messages can be lost.
This is similar to *arrival curve* considered in bounded latency networking
[@latencyBounds].

Starting with description of its apparent properties, we identify
their mathematical definitions, and ultimately arrive at algebra of ΔQ
with basic operations that correspond to abstract interpretations
of network miniprotocols[@NielsenNielsen].

This allows us to use objects from single algebraic body to describe
behaviour of entire protocols as improper CDFs.

Then we discuss expansion of the concept to get most sensitive metrics
of protocol and network robustness.

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
*improper cumulative distribution function* of events arriving over time:

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
We also define *ultimate arrival rate* formally as $a_{u}(ΔQ)=\max(ΔQ)$.
Our improper CDFs are assumed to be always defined within finite subrange
of delays, starting at $0$.

In the following we define domain of *arrival rates* as $\mathcal{A}$,
which is probability, tagged as arrival rate.

We also define domain of *time* or *delays* as $\mathcal{T}$.
We also call a domain of $ΔQ$ functions as
$\mathcal{Q}=\mathcal{T}×(\mathcal{T}\rightarrow{}\mathcal{A})$
^[Pair of deadline, and function from time to arrival rate.].

Below is Haskell specification of this datatype:
```{.haskell .literate}

type Probability = Double -- between 0.0 and 1.0
newtype Delay = Delay Int
newtype Rate = Rate Probability

newtype LatencyDistribution =
  LatencyDistribution {
    -- | Monotonically growing like any CDF. May not reach 1.
    prob :: Series Probability
  }
cdf :: LatencyDistribution -> Series Probability
cdf = cumsum . prob
start = 0
```

We define $start \in{}\mathcal{T}$ as smallest `Delay` (no delay).

## Intuitive properties of ΔQ

We now assume that we have a quality metric $|ΔQ|$, and enumerate properties
it should intuitively exhibit:

1. When shape of our improper CDF function stays the same,
   but arrival rate uniformly
   *drops* by factor $a∈\mathcal{A}$,
   then our quality metric should proportionately drop: $a*|ΔQ(t)|=|a*ΔQ(t)|$.
   (This is a *scalar multiplication* over our domain $\mathcal{Q}$.)
2. When shape of our improper CDF stay the same, but delay uniformly
   *stretches* by factor $d ∈ \mathcal{D}$,
   then our quality metric should proportionately drop: $d*|ΔQ(t)|=|d*ΔQ(t)|$.
   (This is another *scalar multiplication* over our domain $\mathcal{Q}$.)
3. Or we can add a uniform delay $t ∈ \mathcal{D}$, and it will also behave
   like scalar multiplication (we choose this approach as simpler)
   ^[Note that all three can be implemented with the operations from algebra below.]
4. We define a special improper CDF $kept(a)=<0, f(t)=a>$. This indicates
   no delay. We note that $|kept(a_{2})|=a$.
5. We define a special improper CDF that indicates a fixed delay:
   $wait(t)=<t_d, f(t)= \begin{cases} 0 & \text{for } t<t_d \\ 1.0 & \text{for } t=t_d \end{cases}>$.
   This indicates adding a fixed delay. We note that $|wait(t)|=t$.
6. We should have a special CDF that represents *null delay* or *identity*,
   where we pass every message with no delay: $kept(1)=wait(0)=1_{\mathcal{Q}}$.
7. We can say that one $ΔQ$ no worse than the other,
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

These properties give us hints about the metric we should have for $ΔQ$,
scalar multiplications, special instances of the function,
and properties for possible metric.
_Altough metric is yet hard to intuitively define._

We can define properties that natural ΔQ should satisfy:
```
prop_cdf_monotonic = undefined
prop_cdf_deadline_is_nonnegative = undefined
```

## Operations on ΔQ

To model connections of multiple nodes, and network protocols we need two basic
operations:

1. Sequential composition $\mathbf{;}$: given $ΔQ_1(t)$ and $ΔQ_2(t)$ of two different
connections, we should be able to compute the latency function
for routing the message through pair of these connections
in parallel: $$ΔQ(t)=ΔQ_1(t)\mathbf{;}ΔQ_2(t)$$.
    * *associative*:
      $$ΔQ_1(t)\mathbf{;}[ΔQ_2(t)\mathbf{;}ΔQ_3(t)]=[ΔQ_1(t)\mathbf{;}ΔQ_2(t)]\mathbf{;}ΔQ_3(t)$$
    * *commutative*
      $$ΔQ_1(t)\mathbf{;}ΔQ_2(t)=ΔQ_2(t)\mathbf{;}ΔQ_1(t)$$
    * *neutral element* is $1_{\mathcal{Q}}$ or `noDelay`, so:
      $$ΔQ(t)\mathbf{;}1_{\mathcal{Q}}=1_{\mathcal{Q}}\mathbf{;}ΔQ(t)=ΔQ(t)$$

```haskell
rd1 `after` rd2 = LatencyDistribution {
                  --  deadline = deadline   rd1      +     deadline   rd2
                    prob     = prob rd1 `convolve` prob rd2
                  }
```

2. Alternative selection ⊕: given $ΔQ_1(t)$ and $ΔQ_2(t)$ of two different
connections, we should be able to compute the latency function
for routing the message through pair of these connections in parallel:
$ΔQ(t)=ΔQ_1(t) ΔQ_2(t)$.
* *associative*:
  $$ΔQ_1(t)∨[ΔQ_2(t)∨ΔQ_3(t)]=[ΔQ_1(t)∨ΔQ_2(t)]∨ΔQ_3(t)$$
* *commutative*
  $$ΔQ_1(t)∨ΔQ_2(t)=ΔQ_2(t)∨ΔQ_1(t)$$
* *neutral element* is $0_{\mathcal{Q}}$ or `allLost`, so:
  $$ΔQ(t) ∨ 0_{\mathcal{Q}}=0_{\mathcal{Q}}∨ΔQ(t)=ΔQ(t)$$

Here is the Haskell code for naive definition of these two operations:
We can also introduce alternative of two completion rates:
```haskell
rd1 `whicheverIsFaster` rd2 = LatencyDistribution {
  -- deadline = deadline   rd1 `max` deadline   rd2
     prob     = prob rd1 + prob rd2
              - prob rd1 * prob rd2
  }
(∨) = whicheverIsFaster
```
Now let's define neutral elements of both operations above:
```
constantRate = LatencyDistribution {
  -- deadline   = 0
    completion = Series [0]
  }
allLost = keptRate 0.0
noDelay = keptRate 1.0
```
Here:
- `allLost` indicates that no message arrives ever through this connection
- `noDelay` indicates that the all messages always arrive without delay

3. Conjunction of two different actions simultaneously completed in parallel, and waits
until they both are:
```
rd1 `bothComplete` rd2 = LatencyDistribution {
  -- deadline   = deadline   rd1 `max` deadline   rd2
    completion = prob rd1 * cdf2 + prob rd2 * cdf1 - prob rd1 * prob rd2
  }
  where
    cdf1 = cumsum $ prob rd1
    cdf2 = cumsum $ prob rd2
(∧)=bothComplete
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
    initial = prob rdTry `cut` deadline
    remainder = 1 - sum (prob initial)
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
   when we have a little different algebraic properties with *uncut* left argument:
   $$A<t>B=A ∨ \text{wait}_t\mathbf{;}B$$
   $$A<0>B=A ∨ B$$
   $$A<t>\text{fail}=A$$
   $$\text{fail}<t>A=A$$

5. Finally we borrow $μ$ operator to describe iterative miniprotocols
   like unbounded retransmission:
   $$μX.f(X)$$
   Given expression $f(X)$ with free variable $X$, we assume it describes
   the distribution that depends on its own definition $X$.
   Necessary conditions for such definition are that it describes
   at least one terminal value of the ΔQ before
   looping.
```haskell
mu :: LatencyDistribution → LatencyDistribution
mu = undefined
```
   For practical purposes we will rather use bounded iteration:
   $$μ_{n}X[Y].f(X)≡f^{n}(Y)$$.
   Which is $f$ iterated $n$ times, and applied to final value $Y$.
```haskell
iterate :: (LatencyDistribution → LatencyDistribution)
        →   LatencyDistribution → LatencyDistribution
```
This operator seems elegant way of reasoning about infinite retransmission,
where we treat finite description of process that has no bounds on the
number of retransmissions.
Note that probabilities of each occurence of $X$ in $f(X)$ need to scaled down.
In a simple instance where we consider: $$μX.A\mathbf{;}X$$
We just compute
remainder $R=Σ_{0..d(A)}A(i)$, and assume that all occurences of probabilities
in the series $X$ are scaled by $1-R$.
 6. Predictive Network Solutions proposed using operator $A⇆_p B$ for probabilistic
    choice between scenarios $A$ with probability $p$, and $B$ with probability
    $1-p$. This is not necessary, as long as we assume that the only
    way to get non-determinism is due to latency, and our miniprotocols
    involve only deterministic computation, and *unique agency property*
    described by Marcin.

# Defining metrics

Given that we already have three scalar multiplications,
we may derive properties of the metric that is consistent with these:
$$ |A| = Σ_{t∈0..d(A)} \frac{|A(t)|}{t}$$

Note the use of discrete summation instead of integral,
since delay-free loss $loss(x)$ is measurable worsening of the metric,
even though there is only one value of $t=0$ over which to integrate.
^[And as Doron Zeilberger mentions, continuous analysis is only a special case
  of discrete.[@Zeilberger]]

## Estimates
Note that we can define derived estimates of
`LatencyDistribution`
that somewhat approximate it:
```
newtype Deadline = Deadline Delay
deadline :: LatencyDistribution -> Deadline
deadline  = length . prob

newtype Earliest = Earliest Delay
earliest :: LatencyDistribution -> Earliest
earliest  = length . takeWhile (0==) . prob
```
These estimates have the property that we can easily compute
the same operations on estimates, without really computing
the full `LatencyDistribution`.

```
class QualityEstimate e where
  firstToFinish :: e -> e -> e
  lastToFinish  :: e -> e -> e
  andThen       :: e -> e -> e
  -- | Add explicit case/if to make correct estimate
  --   single pass instead of trace
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


...

Our key metric would be diffusion or reachability time of the network $∆R(t)$, which is conditioned
by quality of connection curves $∆Q(t)$ and the structure network graph.

Later in this section, we discuss on how $∆R(t)$ encompasses other plausible
performance conditions on the network.


### Reachability of network broadcast or ∆R(t)

Reachability curve $∆R(t)$ or _diffusion time_ is plotted as rate of nodes
reached by broadcast from committee node, against time.
We want to sum the curve for all possible core nodes.

Area under the curve would tell us the overall network connectivity.
When curve reaches 100% rate, then we have a strongly connected network,
which should be eventually always the case after necessary
reconfigurations.

_Note that when running experiments on multiple networks, we will need to indicate when we show average
statistics for multiple networks, and when we show a statistic for a single network._

### General treatment of completion rate over time

Whether might aim for minimum delay distribution of message
over a given connection $∆Q(t)$ , minimum time of propagation
of the message over entire network ($∆R(t)$, reachability), we still
have a distribution of completion rate over time with standard operations.

We will need a standard library for treating these to speed up our computations.


We can also define a mathematical ring of (rate, delay) pairs.

Note that `LatencyDistribution` is a modulus over ring R with `after` as multiplication,
and `whicheverIsFaster` as addition. Then `noDelay` is neutral element
of multiplication (unit or one), and `bottom` is neutral element of addition.
^[This field definition will be used for multiplication of connection matrices.]
Note that both of these binary operators give also rise to two
almost-scalar multiplication operators:
```
scaleRate  s = after $ constantRate s
scaleDelay s = after $ delay        s
```
This is important, since generalizing the ring of reasonable values
to a field (R, R) gives us opportunity to apply known good graph robustness
algorithms like *effective graph resistance*.


### Description of network connectivity graph in terms of ∆Q

Traditional way of describing graphs is by adjacency matrix,
where 0 means there is no edge, and 1 means that there is active edge.

We may generalize it to unreliable network connections described above,
by using $ΔQ $ instead of binary.

So for each value diagonal, the network connection matrix $A$ will be `noDelay`,
and $A_{i j}$ will represent the connection quality for messages sent from
node $i$ to node $j$.

That allows us to generalize typical graph algorithms executed to
algorithms executed on network matrices:

1. If series $R(A)=\Sigma{}_{n} A+A^2+...+A^n$ converges to matrix of non-zero
(non-`allLost`) values *in all cells* in a finite number of steps, we consider graph to be
*strongly connected*.
Matrix multiplication follows uses $(\mathbf{;},∨)$-modulus.
(So sequential composition in place of multiplication,
  and alternative selection in place of addition.)

Note that this series converges to $ΔQ$ on
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
That corresponds to the situation where given ∆Q(t) ultimately reaches 100%
delivery rate for some delay, ∆R(t) will also always reach 100%.
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
      * visualizing one or multiple rate curves
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

# References

[1] [Peer discovery design considerations](https://docs.google.com/document/d/17cVQoPTd70U7C1lPeHU8w3_C50TJhrVsCYTkPX9YE6I/edit)  [@PeerDiscovery]

[2] [“Network Requirements” - Discussion. Prepared by Neil Davies and Peter Thompson , PNSol.Version 0.1 - 2017-10-10/13](https://input-output-rnd.slack.com/threads/convo/G930386BY-1554166072.004600/)

[3] [Programming Satan's Computer](https://www.cl.cam.ac.uk/~rja14/Papers/satan.pdf) [@ProgrammingSatan]

[5] [Homological Methods in Commutative Algebra. Raghavan, Balwant Singh, Sridharan](http://www.math.tifr.res.in/~publ/pamphlets/homological.pdf) [@HomologicalAlgebra]

[6] [Homological Algebra on Wikipedia](https://en.wikipedia.org/wiki/Homological_algebra)

[7] [Graph measures and network robustness. W. Ellens, R.E. Kooij](https://arxiv.org/pdf/1311.5064.pdf) [@NetworkRobustness]

[8] [Effective resistance...](https://www.nas.ewi.tudelft.nl/people/Piet/papers/LAA_2011_EffectiveResistance.pdf) [@EffectiveGraphResistance]

[9] [The network reliability problem and star semirings. Brent Yorgey. 2016-04-05](https://byorgey.wordpress.com/2016/04/05/the-network-reliability-problem-and-star-semirings/) [@NetworkReliabilityNotSemirings]

[10] [A very general method of computing shortest paths](http://r6.ca/blog/20110808T035622Z.html) [@GeneralMethodOfShortestPaths]

[11] [Transitive closure and related semiring properties via eliminants](https://geomete.com/abdali/papers/TCviaElim.pdf) [@TransitiveClosureSemirings]

# Acknowledgments

Michał started writing, since the task seemed so easy to do within a month
or two.
Neil talked him into defining everything very carefully.
Duncan made sure that discussion constructive proceeds
for reasonable amount of time,
and everybody follows. Peter made sure that the tasks is consistent with
the networking requirements. Marcin made sure that nobody breaks the family
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

```haskell
(|*|) = undefined -- matrix multiplication
frob = undefined -- Frobenius metric
convolve = undefined
fromList = undefined
instance Num Delay where
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  (-) = undefined
instance Ord Delay where
  compare = undefined

instance Num a => Num (Series a) where
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate (Series s)= Series $ negate <$> s

instance Eq Delay where
  (==) = undefined
```

# Appendix: Power series

Here we follow [@PowerSeries]:
```haskell
-- | Put more code here...
data Series a = Series [a]
-- | Cumulative sums computes sums of 1..n-th term of the series
cumsum = undefined
-- | Subtractive remainders computes running remainders to 1.0 after summing up
--   all terms of the series up to given position.
subrem = undefined

completion = undefined
iterate = undefined
cut = undefined
instance Functor Series where
  fmap f (Series a) = Series (fmap f a)
instance Foldable Series where
  foldr f e (Series s) = foldr f e s
instance Semigroup (Series a) where
  Series a <> Series b = Series (a <> b)
```

# References
