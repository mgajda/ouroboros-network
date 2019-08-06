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
import Numeric.Natural

import NullUnit
import qualified Data.Matrix as DM
```
## Square matrices of declared size

This is a simple description of square matrices
with fixed size ^[Note that we considered using `matrix-static`, but it does not have typesafe indexing.].
First we need natural indices that are no larger than $n$:
````{.haskell .literate}
newtype UpTo (n::Nat) = UpTo { unUpTo :: Natural }
  deriving (Eq, Ord, Num)

upTo' :: KnownNat n => Proxy n -> Int -> UpTo n
upTo' n i | i > fromEnum (natVal n) =
  error $ "Value " ++ show i
       ++ " larger than limit of " ++ show (natVal n)
upTo' n i = UpTo (toEnum i)

upTo :: KnownNat n => Int -> UpTo n
upTo = upTo' Proxy

instance KnownNat n => Enum (UpTo n) where
  fromEnum (UpTo n) = fromEnum n
  toEnum            = upTo . toEnum
  enumFromTo (UpTo a) (UpTo b) = UpTo <$> enumFromTo a b
  succ u@(UpTo a :: UpTo n) | a == upToLimit u = error "No successor in UpTo"
  succ (UpTo a :: UpTo n) = UpTo $ succ a

upToLimit :: KnownNat n => UpTo n -> Natural
upToLimit (_ :: UpTo n)= toEnum $ fromIntegral $ natVal (Proxy @n)

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
           => (UpTo n, UpTo n)
           -> SMatrix  n    a
           -> SMatrix (n-1) a
sMakeMinor (i,j) (SMatrix m) = SMatrix (DM.minorMatrix (fromEnum i) (fromEnum j) m)

sFromList :: KnownNat n => Proxy n -> [a] -> SMatrix n a
sFromList (intVal -> n) aList =
  assert (length aList == n*n) $
    SMatrix $
      DM.fromList n n aList

(!) :: KnownNat n => SMatrix n a -> (UpTo n, UpTo n) -> a
(SMatrix m) ! (i,j) = m DM.! (fromEnum i,fromEnum j)

nrows (size -> n) = n
ncols (size -> n) = n

sMatrix :: KnownNat n => Proxy n -> ((UpTo n, UpTo n)->a) -> SMatrix n a
sMatrix (intVal -> n) gen = SMatrix
                          $ DM.matrix n n (\(i,j) -> gen (upTo i, upTo j))
```

We also need to identity and null matrices (for multiplication):
```{.haskell .literate}
instance (KnownNat      n
         ,Null            a)
      =>  Null (SMatrix n a) where
  nullE = sMatrix Proxy (\_ -> nullE)

instance (KnownNat      n
         ,Null            a
         ,Unit            a)
      =>  Unit (SMatrix n a) where
  unitE = sMatrix Proxy elt
    where
      elt (i,j) | i == j = unitE
      elt (i,j)          = nullE
```
