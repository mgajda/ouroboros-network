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
{-# LANGUAGE FlexibleInstances #-}
module Complement where

import Probability
```

### Definition of complement
In different contexts we use a different definition of the complement.
```{.haskell .literate}
class Complement a where
  complement :: a -> a
```
Here we put the definitions.

For probability, there is a well established definition:
```{.haskell .literate}
instance Complement Probability where
  complement p = 1-p
```

For a functor of of objects having complement, there is also well established
definition:
```{.haskell .literate}
instance (Functor    f
         ,Complement   a)
      =>  Complement (f a) where
  complement = fmap complement
```
