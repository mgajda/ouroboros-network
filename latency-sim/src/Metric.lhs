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
## Metrics for approximate comparison
```{.haskell .hidden}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Metric where
```

`Metric` class is a pretty typical for similarity measurement:
```{.haskell .literate}
class Metric a where
  distance :: a -> a -> Double
  similarityThreshold :: Double

infix 3 ~~

(~~) :: Metric t => t -> t -> Bool
(~~) (a::t) (b::t) = distance a b < similarityThreshold @t
```

We will use it for testing, instead of `Eq` which requires equivalence.
