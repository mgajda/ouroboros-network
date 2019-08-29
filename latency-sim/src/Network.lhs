---
input: markdown+tex_math_dollars+yaml_metadata_block+citations
header-includes: |
  \usepackage{bbm}
  \usepackage{bbold}
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
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Network where

import GHC.TypeNats

import Latency
import Metric
import Probability
import SMatrix
```

# Representing networks

Adjacency matrix is classic representation of network graph,
where $i$-th row corresponds to outbound edges of node $i$,
and $j$-th column corresponds to inbound edges of node $j$.
So $A_{i,j}$ is $1$ when edge is connected, and $0$ if edge is not connected.

It is common to store only upper triangular part of the matrix, since:
* it is symmetric for undirected graphs
* it should have $1$ on the diagonal, since every node is connected to itself.

We use this trick to avoid double counting routes with different directions.

So _network connectivity matrix_ is:
* having units on the diagonal
* having connectivitity information between nodes $i$, and $j$, for $j>i$ in element $a_{i,j}$.

Generalizing this to ΔQ-matrices we might be interesting in:
* whether $A^n$ correctly mimicks shortest path between nodes (`Earliest`)
* whether $A^n$ correctly keeps paths shorter than $n$
* for a strongly connected graph there should exist $n≤\mathop{dim}(A)$, such that
  $A^n$ is having non-null elements on an upper triangular section.

More rigorous formulation is:
$$ \begin{array}{rcl}
R_0(A) & = & \mathit{1} \\
R_n(A) & = & R_{n-1}(A)*A \\
\end{array}
$$
Where:

* $\mathit{1}$ or $\mathop{Id}$ denotes a unit adjacency matrix, that is matrix where every node is connected with itself but none else. And these connections have no delay at all.
* $A$ is connection matrix as defined above,
and distribution for a transmission from a single packet from $i$-th to $j$-th
node. For pre-established TCP this matrix should be symmetric.

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

1. If series $R_n(A)$ converges to matrix of non-zero
(non-`allLost`) values *in all cells* in a finite number of steps,
we consider graph to be *strongly connected* [@GeneralMethodOfShortestPaths].
Matrix multiplication follows uses $(\mathbf{;},∨)$ instead of $(*,+)$.
(So sequential composition in place of multiplication,
  and alternative selection in place of addition.)

When it exists, limit of the series $R_n(A)=A^n$ is called $A^{*}$.

In case of non-zero delays, outside diagonal, we may also consider convergence
for delays up to $t$.
NOTE:
  _We need to add estimate of convergence for cutoff time $t$ and number of iterations $n$,
  provided that least delay is in some relation to $n*t$._

Also note that this series converges to $ΔQ$ on
a single shortest path between each two nodes.
That means that we may call this matrix $R_{min}(t)$,
or optimal diffusion matrix.
```{.haskell .literate}
-- | This computes optimal connections on ΔQ matrix.
--   TODO: Check that it works both for boolean matrices, and $\deltaQ{}$.
optimalConnections  :: (Probability                     a
                       ,KnownNat n
                       ,Real                            a
                       ,Metric                          a)
                    => SMatrix   n (LatencyDistribution a)
                    -> SMatrix   n (LatencyDistribution a)
optimalConnections a = converges (fromIntegral $ size a) (|*|a) a

converges ::  Metric r
          =>  Int -- ^ max number of steps
          -> (r -> r)
          ->  r
          ->  r
converges 0      step r = error "Solution did not converge"
converges aLimit step r = if r ~~ r'
                            then                           r
                            else converges (pred aLimit) step r'
  where
    r' = step r
```

We will use this to define *path with shortest ΔQ*.
It corresponds to the situation where all nodes broadcast value from
any starting point $i$ for the duration of $n$ retransmissions. ^[That we do not reduce loss over remainder yet?]

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

We define a matrix multiplication that uses `firstToFinish` in place of addition
and `after` in place of multiplication.
```{.haskell .literate}
(|*|) :: (Probability                     a
         ,KnownNat n                       )
      =>  SMatrix  n (LatencyDistribution a)
      ->  SMatrix  n (LatencyDistribution a)
      ->  SMatrix  n (LatencyDistribution a)
(|*|)  = sMatMult firstToFinish after
```

Note that to measure convergence of the process, we need a notion of distance
between two matrices.

Here, we use Frobenius distance between matrices, parametrized by the notion
of distance between any two matrix elements.
```{.haskell .literate}
instance (Metric            a
         ,KnownNat        n  )
      =>  Metric (SMatrix n a) where
  a `distance` b = sqrt
                 $ sum [square ((a !(i,k)) `distance` (b ! (i,k)))
                         | i <- allUpTo, k<-allUpTo]
    where
      square x = x*x
  similarityThreshold = similarityThreshold @a
```
