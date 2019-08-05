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
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ViewPatterns        #-}
module SMatrix where

import Control.Exception(assert)
import Data.Proxy
import GHC.TypeLits
import GHC.Exts(Proxy#)

import NullUnit
import qualified Data.Matrix as DM
```
## Square matrices of fixed size

This is a simple description of square matrices
with fixed size.

````{.haskell .literate}
data SMatrix (n::Nat) a where
  SMatrix :: KnownNat n => proxy n -> DM.Matrix a -> SMatrix n a
```

```{.haskell .literate}
intVal :: KnownNat n => proxy n -> Int
intVal  = fromInteger . natVal

sMakeMinor ::  KnownNat n
           => (Int, Int)
           -> SMatrix  n    a
           -> SMatrix (n-1) a
sMakeMinor (i,j) (SMatrix (intVal -> n) m) | i>intVal n || j>intVal n = error "Unavailable coordinates"
sMakeMinor (i,j) (SMatrix (intVal -> n) m) = SMatrix Proxy# (DM.minorMatrix i j m)

sFromList :: KnownNat n => Proxy n -> [a] -> SMatrix n a
sFromList (natVal -> n) aList =
  assert (length aList == natVal n*natVal n) $
    DM.fromList n n aList

(SMatrix n _) ! (i,_) | i>intVal n || i<=0 = error $ "Row unavailable:"    ++ show i
(SMatrix n _) ! (_,j) | j>intVal n || j<=0 = error $ "Column unavailable:" ++ show j
(SMatrix n m) ! (i,j) = m ! (i,j)

sNRows (SMatrix n _) = intVal n
sNCols (SMatrix n _) = intVal n

sMatrix n gen = SMatrix n
              $ DM.matrix n n gen
```
Ops to be implemented:
* makeMinor +
* fromList n m [..] +
* (!) +
* matrix n m gen
* nrows
* ncols
