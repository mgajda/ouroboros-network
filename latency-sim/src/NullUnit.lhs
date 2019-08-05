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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module NullUnit where
```
### Nulls and units of multiplication

We are interested in null and unit of multiplication:
```{.haskell .literate}
class Unit a where
  unitE :: a

class Null a where
  nullE :: a

instance Unit Integer where
  unitE = 1

instance Null Integer where
  nullE = 0

instance Unit Int where
  unitE = 1

instance Null Int where
  nullE = 0
```
