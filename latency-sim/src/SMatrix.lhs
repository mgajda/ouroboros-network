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
import Unsafe.Coerce(unsafeCoerce)
```
## Square matrices of declared size

This is a simple description of square matrices
with fixed size ^[Note that we considered using `matrix-static`, but it does not have typesafe indexing.].
First we need natural indices that are no larger than $n$:
```{.haskell .literate}
newtype UpTo (n::Nat) = UpTo { unUpTo :: Natural }
  deriving (Eq, Ord, Num, Typeable)

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
upToLimit (_ :: UpTo n)= toEnum $ fromIntegral $ natVal (Proxy :: Proxy n)

allUpTo'  :: KnownNat n => UpTo n -> [UpTo n]
allUpTo' n = UpTo <$> [1..upToLimit n]

allUpTo :: KnownNat n => [UpTo n]
allUpTo = allUpTo' undefined

newtype SMatrix (n::Nat) a = SMatrix { unSMatrix :: DM.Matrix a }
  deriving (Show, Eq, Functor, Applicative
           ,Foldable, Traversable,  Typeable, Generic)
```

```{.haskell .literate}
size :: KnownNat n => SMatrix n a -> Int
size (s :: SMatrix n a)= intVal (Proxy :: Proxy n)

sizeProxy :: KnownNat n => SMatrix n a -> Proxy n
sizeProxy _ = Proxy

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
Definition of parametrized matrix multiplication is standard, so
we can test it over other objects with defined multiplication and addition-like
operators.
_(We can optimize this definition later, if it turns out to be bottleneck.)_
```{.haskell .literate}
sMatMult ::  KnownNat n
         => (a -> a -> a) -- ^ addition
         -> (a -> a -> a) -- ^ multiplication
         ->  SMatrix  n a
         ->  SMatrix  n a
         ->  SMatrix  n a
sMatMult add mul a1 (a2 :: SMatrix n a) = sMatrix (Proxy :: Proxy n) gen
  where
    gen ::  KnownNat n
        => (UpTo n, UpTo n)
        ->  a
    gen (i,j) = foldr1 add
                       [ (a1 ! (i,k)) `mul` (a2 ! (k,j))
                         | k <- allUpTo' i ]
```
Note that to measure convergence of the process, we need a notion of distance
between two matrices.

Matrix addition for testing:
```{.haskell .literate}
(|+|) :: (Num        a
         ,KnownNat n  )
      => SMatrix   n a
      -> SMatrix   n a
      -> SMatrix   n a
a |+| b = (+) <$> a <*> b
```

Constructing from lists:
```{.haskell .literate}
sMatrixFromLists :: KnownNat n => Proxy n -> [[a]] -> SMatrix n a
sMatrixFromLists p ls = sMatrix p genElt
  where
    genElt (fromEnum . pred -> i
           ,fromEnum . pred -> j) = (ls !! i) !! j
```

For convenient testing in arbitrary dimension,
we need existentially quantified matrices:
```{.haskell .literate}
data SomeSMatrix a =
   forall n. KnownNat             n
          => SomeSMatrix (SMatrix n a)
    deriving (Typeable)

instance Eq a => Eq (SomeSMatrix a) where
  SomeSMatrix a == SomeSMatrix b =
    if size a == size b
      then a == unsafeCoerce b
      else False

instance Show a => Show (SomeSMatrix a) where
  showsPrec _ (SomeSMatrix sm) = ("SomeSMatrix " ++) . shows sm

someSMatrix :: [a] -> SomeSMatrix a
someSMatrix (aList :: [a]) =
    case intSqrt (length aList) of
      Nothing -> error $ "Length of list given to someSMatrix should be a square of a natural number."
      Just n  ->
        let aNat = someNatVal n
        in case aNat of
             Just (SomeNat (theNat :: Proxy n)) ->
               SomeSMatrix $ sMatrixFromList' theNat aList
             Nothing -> error "Impossible in someSMatrix"


sMatrixFromList' :: KnownNat n => Proxy n -> [a] -> SMatrix n a
sMatrixFromList' p@(Proxy :: Proxy n) aList =
  SMatrix $
    DM.fromList (intVal p) (intVal p) aList
--                   (SMatrix :: [a] -> SMatrix n a)
--                  DM.fromList (intVal theNat) (intVal theNat) aList
sMatrixFromList :: KnownNat n => [a] -> SMatrix n a
sMatrixFromList  = sMatrixFromList' Proxy

intSqrt :: Int -> Maybe Integer
intSqrt i = case iSqrt i of
              n | n*n==fromIntegral i -> Just n
              otherwise               -> Nothing
  where
    iSqrt :: Int -> Integer
    iSqrt = floor . sqrt . fromIntegral
```

One might also want to iterate over rows or columns in the matrix:
```{.haskell .literate}
rows, columns :: KnownNat n
              => SMatrix n a -> [[a]]
rows    sm = [[sm ! (i,j) | j<-allUpTo ] | i<-allUpTo]
columns sm = [[sm ! (i,j) | i<-allUpTo ] | j<-allUpTo]
```
