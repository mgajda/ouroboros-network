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
4. We can say that one $ΔQ$ no worse than the other,
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

# Defining metrics

Given that we already have three scalar multiplications,
we may derive properties of the metric that is consistent with these:
$$ |A| = Σ_{t∈0..d(A)} \frac{|A(t)|}{t}$$

Note the use of discrete summation instead of integral,
since delay-free loss $loss(x)$ is measurable worsening of the metric,
even though there is only one value of $t=0$ over which to integrate.
^[And as Doron Zeilberger mentions, continuous analysis is only a special case
  of discrete.[@Zeilberger]]


### Description of network connectivity graph in terms of ∆Q

Considering two nodes we may consider delay introduced by
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


## What for capacity-limited networks?

Our environment will compute maximum flow of messages sent between
nodes at each point of time.
Our proofs can also use constant bound on the number of messages
sent between nodes at each step of the protocol.
That should assure that our capacity-insensitive approximations
stay valid for all networks that have significantly more capacity
than used by the algorithm.


# References
