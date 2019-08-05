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
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import GHC.Exts(Proxy#)

import NullUnit
import qualified Data.Matrix as DM
```
## Square matrices of ©ed size

This is a simple description of square matrices
with fixed size.

````{.haskell .literate}
newtype SMatrix (n::Nat) a = SMatrix { unSMatrix :: DM.Matrix a }
  deriving (Show, Eq, Functor, Applicative
           ,Foldable, Traversable, Typeable, Generic)
```

```{.haskell .literate}
size :: KnownNat n => SMatrix n a -> Int
size (s :: SMatrix n a)= intVal (Proxy @n)

intVal :: KnownNat n => Proxy n -> Int
intVal = fromIntegral . natVal

sMakeMinor ::  KnownNat n
           => (Int, Int)
           -> SMatrix  n    a
           -> SMatrix (n-1) a
sMakeMinor (i,j) (size -> n) | i>n || j>n = error "Unavailable coordinates"
sMakeMinor (i,j) (SMatrix m) = SMatrix (DM.minorMatrix i j m)

sFromList :: KnownNat n => Proxy n -> [a] -> SMatrix n a
sFromList (intVal -> n) aList =
  assert (length aList == n*n) $
    SMatrix $
      DM.fromList n n aList

(size -> n) ! (i,_) | i>n || i<=0 = error $ "Row unavailable:"    ++ show i
(size -> n) ! (_,j) | j>n || j<=0 = error $ "Column unavailable:" ++ show j
(SMatrix m) ! (i,j)               = m DM.! (i,j)

nrows (size -> n) = n
ncols (size -> n) = n

sMatrix :: KnownNat n => Proxy n -> ((Int, Int)->a) -> SMatrix n a
sMatrix (intVal -> n) gen = SMatrix
                          $ DM.matrix n n gen
```
Ops to be implemented:
* makeMinor +
* fromList n m [..] +
* (!) +
* matrix n m gen
* nrows
* ncols
