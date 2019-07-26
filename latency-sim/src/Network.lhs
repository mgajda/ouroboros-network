```{.haskell .hidden}
module Network where

import Control.Exception(assert)
import Data.Matrix

import Latency
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
```{.haskell .literate}
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

```{.haskell .literate}
-- | Matrix multiplication
(|*|) :: TimeToCompletion a => Matrix a -> Matrix a -> Matrix a
(|*|) = matMult firstToFinish after
-- | Given an addition and multiplication operations, derive matrix multiplication
--   operator.
-- TODO: Add checking dimensions
-- Quick inefficient definition, assuming we spend most of the time multiplying
-- values
matMult :: (a -> a -> a) -> (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
matMult add mul a1 a2 = assert (n' == m) $
    matrix n m' $ \(i,j) -> foldr1 add [ (a1 ! (i,k)) `mul` (a2 ! (k,j)) | k <- [1 .. m] ]
  where
    n  = nrows a1
    m  = ncols a1
    n' = nrows a2
    m' = ncols a2
frob = undefined -- any reasonable metric on the matrix, like frobenius metric
```
