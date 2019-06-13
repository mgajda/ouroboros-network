---
authors: |
  Michał J. Gajda, Duncan Coutts, Neil Davies,
  Marcin Szamotulski
title: Topology Discovery
description: |
  Task specification for the topology discovery
  design and simulation.
date: May 10th 2019, v1.5
input: markdown
output:
  pdf_document:
    toc: true
    mainfont: "DejaVu Serif"
    sansfont: Arial
    latex_engine: xelatex
---

# Task specification

We want to simulate Cardano network, with nodes joining,
and leaving at random times. We want to compute data diffusion effectiveness
of the network, and time needed to broadcast the new state of the network after each change.


There will be up to^[Numbers are just for orientation.]:

* 100-1k core nodes (also called stake pools^[Each network has to have at least one stake pool])
  - that know about each other stake pool
  - that are widely known (and thus vulnerable)
  - their goal is to optimize network reachability ("routing")
* 100k-1M leaf (edge) nodes
  - those that do not participate in routing

Our key performance metric will be plots of cumulative probability
mass functions $Q(t)$:

* $∆Q(t)$ - quality of a single connection as detected by the lower level communication layer (input)
* $∆R(t)$ or reachability curve - connectivity of the entire network (benchmark for the output)

We call the graph of all connections *probed* to get $∆Q(t)$
statistics as *known connectivity graph* or *topology*.
We call the graph of actually used connections the
*active connections graph* or *topography* of the system.

## Performance metrics

Our key metric would be diffusion or reachability time of the network $∆R(t)$, which is conditioned
by quality of connection curves $∆Q(t)$ and the structure network graph.

Later in this section, we discuss on how $∆R(t)$ encompasses other plausible
performance conditions on the network.

### Quality of the connection curve ∆Q(t)

We plot rate of messages arriving before deadline $t$:

<center>

![Completion rate against deadline](completion-rate.png "Completion rate against deadline"){.center}\

</center>

Note that (say) only 99% (for failure rate $ε=1%$ ) of messages arrive
at all within desired time $t$ ,
and we silently drop those that arrive later.
(We might need to perform TCP analysis to bound resource
  consumption of late messages. Possibly decrease
  recommended TCP timeout on the nodes - in order to
  drop the connections that are failing for too long.)

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

Below is Haskell specification of this datatype:
```{.haskell .literate}

type Probability = Double -- between 0.0 and 1.0
newtype Delay = Delay NominalDiffTime
newtype Rate = Rate Probability

data RateDistribution =
  RateDistribution {
    -- | Deadline after which we drop messages (or ignore them).
    deadline   :: Delay
    -- | Monotonically growing like any CDF. May not reach 1.
  , completion :: Delay -> Rate
  }
```

Now we can introduce sequential composition of two completion rates:
```haskell
rd1 `after` rd2 = RateDistribution {
                    deadline   = deadline rd1 + deadline rd2
                  , completion = completion rd1 `convolve` completion rd2
                  }
```
We can also introduce alternative of two completion rates:
```haskell
rd1 `whicheverIsFaster` rd2 = RateDistribution {
    deadline   = deadline rd1 `max` deadline rd2
  , completion = completion rd1 `max` completion rd2
  }
```
Now let's define neutral elements of both operations above:
```
constantRate = RateDistribution {
    deadline   = 0
  , completion = \_->0.0
  }
bottom  = constantRate 0.0
noDelay = constantRate 1.0
```
Here:
- `bottom` indicates that no message arrives ever through this connection
- `noDelay` indicates that the all messages always arrive without delay

We can also define a mathematical ring of (rate, delay) pairs.

Note that `RateDistribution` is a modulus over ring R with `after` as multiplication,
and `whicheverIsFaster` as addition. Then `noDelay` is neutral element
of multiplication (unit or one), and `bottom` is neutral element of addition.
^[This field definition will be used for multiplication of connection matrices.]
Note that both of these binary operators give also rise to two
almost-scalar multiplication operators:
```haskell
scaleRate  s = after $ constantRate s
scaleDelay s = after $ delay        s
```
This is important, since generalizing the ring of reasonable values
to a field (R, R) gives us opportunity to apply known good graph robustness
algorithms like *effective graph resistance*.

Final conceptual operation would be to wait for two different messages to
reach us (a linear conjunction):
```haskell
rd1 `bothComplete` rd2 = RateDistribution {
    deadline   = deadline rd1 `max` deadline rd2
  , completion = completion rd1 `min` completion rd2
  }
```

### Description of network connectivity graph in terms of ∆Q

Traditional way of describing graphs is by adjacency matrix,
where 0 means there is no edge, and 1 means that there is active edge.

We may generalize it to unreliable network connections described above,
by using $\delta{}Q{}$ instead of binary.

So for each value diagonal, the network connection matrix $A$ will be `noDelay`,
and $A_{i j}$ will represent the connection quality for messages sent from
node $i$ to node $j$.

That allows us to generalize typical graph algorithms executed to
algorithms executed on network matrices:

1. If series $R(A)=\Sigma{}_{n} A+A^2+...+A^n$ converges to matrix of non-zero
(non-`bottom`) values in a finite number of steps, we consider graph to be
*strongly connected*.
Note that this series converges to $\delta{}Q$ on
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
           then     r
           else fix r'
  where
    r' = step r
    almostEqual a1 a2 = modulus (a-a2)<=epsilon
      where
        epsilon=0.001
        -- | Any reasonable measure of matrix divergence
        modulus = maximum . fmap abs . fromList
```
2. We need to consider further examples of how our metrics react
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

## Discovery policies

We can describe peer discovery policies as
[Pregel](https://kowshik.github.io/JPregel/pregel_paper.pdf)-like programs
on graphs. These are:

* executed in iterations;
* with each step executed simultaneously on all nodes;
* each node can only access its local (and bounded) state,
and send/receive messages to its local neighbours;
* nodes can receive an _address_ of another node;
* for each address, node can establish connection with its node
in the next address.

Ultimately we want to execute these programs *asynchronously*
to optimize their speed, but initial simulations will be lockstep
(synchronous) among node iterations.

```haskell
-- | Address of another node in the graph
newtype Address

-- | Action in a single policy iteration
data Action state = Action {
    result     :: state
  , connect    :: [Address]
  , disconnect :: [Address]
  }

-- | Policy as a functional program executed *on each node*.
type Policy state =
  (  state, -- ^ Local state of the node
    [state] -- ^ Neighbour states
  ) -> Action state
```

[Pregel model](https://kowshik.github.io/JPregel/pregel_paper.pdf) is
the most general with its node-centric abstraction among currently considered
[graph processing models](https://arxiv.org/pdf/1607.02646.pdf).

### Simulating policies

For each policy we may try to simulate failures by one of the following:

1. Random nodes go dark.
2. Random nodes generate random disruption at random times.
3. Random nodes join "disruptive party" that uses *disruption policy*.
   - This policy can simulate each major attack type that may occur
     on other blockchains.
   - This policy would then be optimized to get worst disruption within the
     network.
   - We assume that *disruptive nodes* can explicitly recognize each other.

_Ad 1 and 2:_  Here both 1 and 2 give credible assessment of behaviour of the algorithm
in the normal failure.

_Ad 3:_ However 3 is the key to defend against targeted attacks by a powerful adversary.
Note that to simplify design of the *disruption policy*, we assume
that disruptive nodes will always know whether the other node is disruptive,
but not _vice versa_. (Disruptive nodes are unknown to honest nodes.)

### Simulation questions

1. Do we need to somehow share information about entire $\delta{}Q$
  within the network?
    * How to deal with possible lies about it?
2. Does *disruptive party* need to communicate to achieve its goal?
3. Does *disruptive party* need some global communication?
4. What are the threshold rates to achieve the convergence of $\delta{}R(t)$.

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

1. Research on best metrics for latency-driven capacity-insensitive applications:
  a) possible metrics
  b) comparison with other graph metrics
  c) properties of the metric
    * enumeration of desired or expected properties
    * derive algebra of the chosen metric
  d) implementation of the metric
    * datatype for $∆Q(t)$ and $∆R(t)$.
      and optimization of reduction of $∆Q(t)$
      computation.
    * computing $∆Q(t)$ and $∆R(t)$ curve for random network
      connections.
  e) comparison with other metrics used for graph and network research
    * visualizing one or multiple rate curves
    * assertions about classical graph algorithms vs rates
    * computing metrics random networks
2. Topology discovery
  * simulator of the network
  * policy DSL as Pregel-like programs with only local knowledge of the node
  * implementation of simple policies to probe network graph
  * computing metrics on the network
  * compute statistical distribution of metrics for random networks
3. Connection optimization
  * finding the best way to send messages given a local knowledge of the network
  * statistical evaluation of dynamically adding and subtracting nodes
4. Adversarial scenarios
  * implement adversary policies corresponding to known network attacks
  * show behaviour of the network on the adversarial policies
  * attempt to prove lower bound on reachability within the network
  * attempt to get static bounds on number of messages sent between each pair
    of nodes in each cycle
5. Implement miniprotocol for the peer discovery

## Desired outcomes:
* mathematical theory and statistical tool for modelling tool latencies
  over high capacity networks
* proposals of reasonable peer discovery policies
* statistical assessments of these policies under adverse scenarios

## Future plans:
* proof that our policies are almost-optimal after convergence
* expand the theory into capacity-limited networks
* apply it to reason about latency of other protocols within Cardano

## Example scenarios

1. We start with ~21 on current Cardano topology, and randomly add
and subtract nodes to grow the network.
2. We expect [coronal mass ejection](https://en.wikipedia.org/wiki/Coronal_mass_ejection) to bring down significant
part of Cardano network for entire day, and show that remaining
part is still responsive *or* it blocks the service due
to insufficient stake.
3. We emulate breach of all [transatlantic cables](https://en.wikipedia.org/wiki/Transatlantic_communications_cable), and check
how long it take to reestablish the connection through remaining
network.
4. Given a fixed random rate of adding and subtracting nodes,
we simulate the difference between the statistic of current
network, and shortest propagation time network.
5. Known (stake pool) node is attacked by random new nodes trying to connect to them all the time to exhaust their resources.
6. Node is attacked by random new nodes with high $∆Q$ that try to connect to it in order to slow its overall connection speed.
7. Attacker pays for few well-connected nodes in order to get a capability to split and indefinitely delay the network.
8. Lot of random nodes connect and disconnect from the node in order to exhaust its resources.

# References

[1] [Peer discovery design considerations](https://docs.google.com/document/d/17cVQoPTd70U7C1lPeHU8w3_C50TJhrVsCYTkPX9YE6I/edit)

[2] [“Network Requirements” - Discussion. Prepared by Neil Davies and Peter Thompson , PNSol.Version 0.1 - 2017-10-10/13](https://input-output-rnd.slack.com/threads/convo/G930386BY-1554166072.004600/)

[3] [Pregel: A System for Large-Scale Graph Processing](https://kowshik.github.io/JPregel/pregel_paper.pdf)

[4] [High-Level Programming Abstractions for Distributed Graph Processing](https://arxiv.org/pdf/1607.02646.pdf)

[5] [Programming Satan's Computer](https://www.cl.cam.ac.uk/~rja14/Papers/satan.pdf)

[6] [Matrix completion problems over integral domains. Borobia and Canogar](https://doi.org/10.1016/j.laa.2011.06.049)

[7] [Some applications of linear algebra over finite fields. Simeon Ball](http://www2.ims.nus.edu.sg/Programs/011code/files/ball_ab.pdf)

[8] [Scalar products of elementary distributions. Philippe Droz-Vincent](https://arxiv.org/pdf/0707.0974.pdf)

[9] [Homological Methods in Commutative Algebra. Raghavan, Balwant Singh, Sridharan](http://www.math.tifr.res.in/~publ/pamphlets/homological.pdf)

[10] [Homological Algebra on Wikipedia](https://en.wikipedia.org/wiki/Homological_algebra)

[11] [Graph measures and network robustness. W. Ellens, R.E. Kooij](https://arxiv.org/pdf/1311.5064.pdf)

[12] [Effective resistance...](https://www.nas.ewi.tudelft.nl/people/Piet/papers/LAA_2011_EffectiveResistance.pdf)

[13] ["Network implementation overview"](https://docs.google.com/document/d/1qwFnHXgLQArph5FdxfJMrPfn8Y5KhozYU8i5R6gVZbo/edit#heading=h.pd8vpckcfb2u)

# Acknowledgments

Michał started writing, since the task seemed so easy to do within a month
or two.
Neil talked him into defining everything very carefully.
Duncan made sure that discussion constructive proceeds
for reasonable amount of time,
and everybody follows. Peter made sure that the tasks is consistent with
the networking requirements. Marcin made sure that nobody breaks the family
glasses. Knut provided online source of temptations to delay release.

# Glossary

* $t$ - time since sending the message, or initiating a process
* $∆Q(t)$ - response rate of a single connection after time $$t$$
  (chance that message was received until time $$t$$)
* $∆R(t)$ - completion rate of broadcast to entire network (rate of nodes
  expected to have seen the result until time $$t$$)
* $ε$ - rate of packets that are either dropped or arrive after
  latest reasonable deadline we chose
* *full connectivity graph* - all available connections between node
  (in *absence* of firewalls and network glitches,
  [complete graph also known as clique](https://en.wikipedia.org/wiki/Complete_graph))
  It includes both *cold*, *warm* and *hot* connection.
* *hot connections* or *active connections graph* - those connections that our policy
  maintains as active connections
* *warm connections* - connections known to their peers, but unused
* *cold connections* - connections that are part of *full graph* of possible
  connections, but have not been yet probed by their peers (or knowledge about
  past probing was forgotten),
* *policy* - algorithm for opening and closing active connections to Cardano
  network, executed on each node
