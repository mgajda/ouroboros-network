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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ViewPatterns        #-}
module SMatrix where

import GHC.TypeNats

import NullUnit
import Data.Matrix
```
## Square matrices of fixed size

This is a simple description of square matrices
with fixed size.

```{.haskell .literate}
data SMatrix (n::Nat) where
  SMatrix :: (n::Nat) -> ((Nat,Nat)->a) -> SMatrix n a
```
