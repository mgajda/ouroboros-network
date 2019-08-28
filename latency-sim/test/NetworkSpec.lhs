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
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module NetworkSpec where

import Free
import Latency
import Metric
import Network
import NullUnit
import Probability
import Series
import SMatrix
import LatencySpec

import Control.Monad
import Control.Exception(assert)
-- import Data.Matrix
import Data.Proxy
import Data.Ratio
import Data.Validity
import Data.Foldable(fold)
import GHC.TypeNats

import Test.Hspec hiding(after)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Test.Validity
import Test.Validity.Operations.Associativity
import Test.Validity.Operations.Commutativity
import Test.Validity.Operations.Identity
```

# Appendix: validation of operations on the networks and matrices

Note that in the context of this work, we are only interested
in connection matrices. They have the following properties:
* they are always square matrices
* they always have unit distribution (`noDelay`) on the diagonal

```{.haskell .literate .ignore}
instance (Unit            a
         ,Eq              a
         ,Validity        a)
      => Validity (Matrix a) where
  validate a = mconcat
      [connMatrixElement i k | i<-[1..n], k<-[1..m]]
    where
      n = nrows a
      m = ncols a
      connMatrixElement i k | i == k =
        (a ! (i,k) == unitE) `check` "diagonal element is unit"
      connMatrixElement i k | i /= k =
        validate (a ! (i,k))

instance (Unit              a
         ,Arbitrary         a)
      =>  Arbitrary (Matrix a) where
  arbitrary = do
      n <- choose (1,20) -- No change expected for larger matrices
      genConnMatrix n

  shrink a | nrows a == 1 = []
  shrink a = makeMinor <$> indices a
    where
      makeMinor (i, k) = minorMatrix i k a
      n = nrows a
      m = ncols a
```
To generate connection matrix of size $n$:
```{.haskell .literate .ignore}
genConnMatrix :: (Arbitrary  a
                 ,Unit       a)
              => Int
              -> Gen (Matrix a)
genConnMatrix n = do
  fromList n n <$> -- we are interested in square matrices only
    sequence [genConnElt i k | i<-[1..n]
                             , k<-[1..n]]
```
When we are interested in generating multiple matrices of the same size,
we can generate a generate for a random size:
```{.haskell .literate .ignore}
genGenConnMatrix :: (Unit               a
                    ,Null               a
                    ,Arbitrary          a)
                 =>  Gen (GenIN (Matrix a))
genGenConnMatrix  = do
  n <- choose (1,20)
  return $ GenIN { genTest = genConnMatrix n
                 , genUnit = unitSized     n
                 , genNull = nullSized     n
                 }

swapGenConnMatrix genGen = do
  GenIN {..} <- genGen
  return $ GenIN { genTest = genTest
                 , genUnit = genNull
                 , genNull = genUnit
                 }

unitSized n = matrix n n unitElt

nullSized n = matrix n n $ const nullE

unitElt (i,j) | i==j = unitE
unitElt _            = nullE
```

Generation of element in connection matrix depends on index:
```{.haskell .literate .ignore}
genConnElt i j | i == j = return unitE
genConnElt _ _          = arbitrary

-- elements a = map (a!) $ indices a

indices a = [(i,k) | i<-allUpTo
                   , k<-allUpTo

instance CoArbitrary         a
      => CoArbitrary (Matrix a) where
  coarbitrary = foldr (.) id
              . fmap coarbitrary

instance (Arbitrary           a
         ,Unit                a)
      => GenUnchecked (Matrix a) where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink

instance (Unit             a
         ,Eq               a
         ,Validity         a
         ,Arbitrary        a)
      =>  GenValid (Matrix a) where
  genValid = arbitrary `suchThat` isValid
```

Here we have properties typical of traditional instances of `Num`:
```{.haskell .literate .ignore}
identityWithGen :: Gen (Gen a, a) -> Property
identityWithGen = undefined

specNegateIsSelfAdjoint x = negate (negate x) == x

-- | Generator of both generator, null, and identity.
data GenIN a =
  GenIN {
    genTest :: Gen a
  , genUnit :: a
  , genNull :: a
  }

specACIOnGenGen :: forall a. (Show      a
                             ,Eq        a
                             ,Arbitrary a
                             ,GenValid  a)
                => (a -> a -> a)
                -> Gen (GenIN a)
                -> String
                -> SpecWith ()
specACIOnGenGen op gen description =
    describe description $ do
      --prop "commutativity"             $
      --  commutativeOnGens op   pairGen    pairShrink
      prop "associativity"                   $
        associativeOnGens op   tripleGen  tripleShrink
      prop "neutral element" $ -- this needs better types
        identityWithGenIN op gen
      --prop "null element" $ -- this needs better types
      --  nullWithGenIN     op gen shrink
  where
    pairShrink :: (a,a) -> [(a,a)]
    pairShrink   (a,b)   = zip  (shrink a) (shrink b)
    tripleShrink (a,b,c) = zip3 (shrink a) (shrink b) (shrink c)
    pairGen              = do
      GenIN {genTest} <- gen
      (,)  <$> genTest <*> genTest
    tripleGen            = do
      GenIN {genTest} <- gen
      (,,) <$> genTest <*> genTest <*> genTest
    specialGen = undefined

nullWithGenIN     op genIn shrink = do
  GenIN {genTest, genNull} <- genIn
  aTest <- genTest
  assert   ((aTest   `op` genNull) == genNull) $
    return ((genNull `op` aTest  ) == genNull)

identityWithGenIN op genIn = do
  GenIN {genTest, genUnit} <- genIn
  aTest <- genTest
  assert   ((aTest   `op` genUnit) == aTest) $
    return ((genUnit `op` aTest  ) == aTest)

specAddOnGen :: forall a. (Show      a
                          ,Eq        a
                          ,Num       a
                          ,Arbitrary a
                          ,GenValid  a)
             => Gen (GenIN a) -> SpecWith ()
specAddOnGen genIN = specACIOnGenGen (+) genIN "addition"

```
Or should I use `forAllShrink :: (Show a, Testable prop) => Gen a -> (a -> [a]) -> (a -> prop) -> Property`?
```{.haskell .literate .ignore}
specMulOnGen :: forall a. (Show      a
                          ,Eq        a
                          ,Num       a
                          ,Arbitrary a
                          ,GenValid  a)
             => Gen (GenIN a)
             -> SpecWith ()
specMulOnGen gen = specACIOnGenGen (*) gen "multiplication"

a `sameDim` b = (nrows a == nrows b)
```

Test that type class instances are valid:
```{.haskell .literate}
spec = do
  {-describe "check properties of integers" $ do
    specAddOnGen  $ pure $ GenIN (arbitrary :: Gen Integer) 0 0
    specMulOnGen  $ pure $ GenIN (arbitrary :: Gen Integer) 1 0
  describe "check properties of matrices of integers" $ do
    specAddOnGen  $ swapGenConnMatrix $ genGenConnMatrix @Integer
    specMulOnGen  $ genGenConnMatrix @Integer-}
  describe "examples" $ do
    describe "symbolic" $ do
      examples ("d" :: FreeTTC) nullE unitE
    describe "LatencyDistribution" $ do
      examples [0, 0.45, 0.45]
               [0] ([1]::LatencyDistribution IdealizedProbability)

```

We also specialize the types for testing
```{.haskell .literate}
(/*>) :: KnownNat n
      => SMatrix  n (LatencyDistribution ApproximateProbability)
      -> SMatrix  n (LatencyDistribution ApproximateProbability)
      -> SMatrix  n (LatencyDistribution ApproximateProbability)
(/*>)  = sMatMult firstToFinish after
(***) :: KnownNat n
      => SMatrix  n (LatencyDistribution IdealizedProbability)
      -> SMatrix  n (LatencyDistribution IdealizedProbability)
      -> SMatrix  n (LatencyDistribution IdealizedProbability)
(***)  = sMatMult firstToFinish after

```

Here are the examples discussed along with distributivity:
```{.haskell .literate}
infixr 7 ♢
infixr 5 ∨

(♢), (∨), (∧) :: TimeToCompletion ttc
               => ttc -> ttc -> ttc
(♢) = after
(∨) = firstToFinish
(∧) = lastToFinish

examples :: (Metric           w
            ,TimeToCompletion w
            ,Show             w)
         => w -> w -> w -> Spec
examples (d::w) (_0::w) (_1::w)= do
    describe "directed" $ do
      describe "1 directed" $ do
        it "Second iteration" $
          ex1_1 *** ex1_1                       `shouldBeSimilar` ex1_2
        it "Third iteration" $
          ex1_1 *** ex1_1 *** ex1_1             `shouldBeSimilar` ex1_3
      describe "2" $ do
        it "Second iteration" $
          ex2_1 *** ex2_1                       `shouldBeSimilar` ex2_2
        it "Third iteration" $
          ex2_1 *** ex2_1 *** ex2_1             `shouldBeSimilar` ex2_3
      describe "3" $ do
        it "First iteration" $
          ex3_1 *** ex3_1                       `shouldBeSimilar` ex3_2
        it "Second iteration" $
          ex3_1 *** ex3_1 *** ex3_1             `shouldBeSimilar` ex3_3
        it "Third iteration" $
          ex3_1 *** ex3_1 *** ex3_1 *** ex3_1   `shouldBeSimilar` ex3_4
    describe "directed with self-connectivity" $ do
      describe "4" $ do
        it "First iteration" $
          ex4_1 *** ex4_1                       `shouldBeSimilar` ex4_2
      describe "5" $ do
        it "First iteration" $
          ex5_1 *** ex5_1                         `shouldBeSimilar` ex5_2
        {-it "Second iteration" $
          ex5_1 *** ex5_1 *** ex5_1               `shouldBeSimilar` ex5_3-}
  where
    (***) :: KnownNat n
          => SMatrix  n w
          -> SMatrix  n w
          -> SMatrix  n w
    (***)  = sMatMult firstToFinish after
```
```dot
digraph example1{
  rankdir=LR;
  graph [nodesep=2];
  node [shape=plaintext];
  1 -> 2;
  2 -> 3;
}
```
```{.literate .haskell}
    ex1_1, ex1_2, ex1_3 :: SMatrix 3 w
    ex1_1 =
          sMatrixFromLists (Proxy :: Proxy 3)
                   [[_0, d,_0]
                   ,[_0,_0, d]
                   ,[_0,_0,_0]
                   ]
    ex1_2 = sMatrixFromLists (Proxy :: Proxy 3)
                   [[_0,_0, d♢d]
                   ,[_0,_0,_0]
                   ,[_0,_0,_0]
                   ]
    ex1_3 = sMatrixFromLists (Proxy :: Proxy 3)
                    [[_0,_0,_0]
                    ,[_0,_0,_0]
                    ,[_0,_0,_0]
                    ]

```
```dot
digraph example2 {
  rankdir=BT;
  graph [nodesep=2];
  node [shape=plaintext];
  subgraph a {
    2;
    3;
    rank=same;
  }
  1 -> 2;
  1 -> 3;
  2 -> 3;
  2 -> 4;
  3 -> 4;
}
```
```{.literate .haskell}
    ex2_1, ex2_2, ex2_3 :: SMatrix 4 w
    ex2_1 =
      sMatrixFromLists (Proxy :: Proxy 4)
               [[_0, d, d,_0]
               ,[_0,_0, d, d]
               ,[_0,_0,_0, d]
               ,[_0,_0,_0,_0]
               ]
    ex2_2 =
       sMatrixFromLists (Proxy :: Proxy 4)
                [[_0,_0, d♢d,(d♢d)∨(d♢d)]
                ,[_0,_0,  _0, d♢d        ]
                ,[_0,_0,  _0,_0          ]
                ,[_0,_0,  _0,_0          ]
                ]
    ex2_3 =
       sMatrixFromLists (Proxy :: Proxy 4)
                [[_0,_0, _0, d♢d♢d]
                ,[_0,_0, _0, _0]
                ,[_0,_0, _0, _0]
                ,[_0,_0, _0, _0]
                ]
    ex2_4 =
       sMatrixFromLists (Proxy :: Proxy 4)
                [[_0,_0, _0,_0]
                ,[_0,_0, _0,_0]
                ,[_0,_0, _0,_0]
                ,[_0,_0, _0,_0]
                ]
```
```dot
digraph example3{
  rankdir=LR;
  graph [nodesep=2];
  node [shape=plaintext];
  1 -> 2;
  2 -> 3;
  2 -> 4;
  3 -> 4;
}
```
```{.literate .haskell}
    ex3_1, ex3_2, ex3_3, ex3_4 :: SMatrix 4 w
    ex3_1 =
      sMatrixFromLists (Proxy :: Proxy 4)
               [[_0, d,_0,_0]
               ,[_0,_0, d, d]
               ,[_0,_0,_0, d]
               ,[_0,_0,_0,_0]
               ]
    ex3_2 = sMatrixFromLists (Proxy :: Proxy 4)
                       [[_0,_0,d♢d ,d♢d]
                       ,[_0,_0,_0   ,d♢d]
                       ,[_0,_0,_0  ,_0 ]
                       ,[_0,_0,_0  ,_0 ]
                       ]
    ex3_3 = sMatrixFromLists (Proxy :: Proxy 4)
                      [[_0,_0,_0, d♢d♢d]
                      ,[_0,_0,_0, _0]
                      ,[_0,_0,_0 ,_0 ]
                      ,[_0,_0,_0 ,_0 ]
                      ]
    ex3_4 = sMatrixFromLists (Proxy :: Proxy 4)
                      [[_0,_0,_0,_0]
                      ,[_0,_0,_0,_0]
                      ,[_0,_0,_0,_0]
                      ,[_0,_0,_0,_0]
                      ]
```
```dot
digraph Ex4 {
  rankdir=BT;
  graph [nodesep=2];
  node [shape=plaintext];
  subgraph a {
    2;
    3;
    rank=same;
  }
  1 -> 2 -> 3 -> 4;
  2 -> 4;
  1 -> 3;
  1 -> 1;
  2 -> 2;
  3 -> 3;
  4 -> 4;
}
```
```{.literate .haskell}
    ex4_1, ex4_2 :: SMatrix 4 w
    ex4_1 =
      sMatrixFromLists (Proxy :: Proxy 4)
               [[_1, d, d,_0]
               ,[_0,_1, d, d]
               ,[_0,_0,_1, d]
               ,[_0,_0,_0,_1]
               ]
    ex4_2 = sMatrixFromLists (Proxy :: Proxy 4)
                       [[_1,d∨d,d∨d∨d♢d,d♢d∨d♢d]
                       ,[_0,_1,d∨d     ,d∨d∨d♢d]
                       ,[_0,_0,      _1,d∨d     ]
                       ,[_0,_0,      _0,_1      ]
                       ]
```
```dot
digraph Ex5 {
  rankdir=LR;
  graph [nodesep=2];
  node [shape=plaintext];
  1 -> 1;
  2 -> 2;
  3 -> 3;
  4 -> 4;
  1 -> 2 -> 3 -> 4;
  2 -> 4;
}
```
```{.literate .haskell}
    ex5_1, ex5_2 :: SMatrix 4 w
    ex5_1 =
     sMatrixFromLists (Proxy :: Proxy 4)
              [[_1, d,_0,_0]
              ,[_0,_1, d, d]
              ,[_0,_0,_1, d]
              ,[_0,_0,_0,_1]
              ]
    ex5_2 = sMatrixFromLists (Proxy :: Proxy 4)
                      [[_1,d∨d, d♢d,     d♢d]
                      ,[_0, _1, d∨d ,d∨d∨d♢d]
                      ,[_0, _0,  _1 ,d∨d     ]
                      ,[_0, _0,  _0 ,_1      ]
                      ]
    {-ex5_3 = sMatrixFromLists (Proxy :: Proxy 4)
                      [[_1,d∨d∨d,  d♢d∨d♢(d∨d), d♢d∨d♢d∨d♢(d∨d)∨(d∨d)♢(d∨d)    ]
                      ,[_0,    _1,        d∨d∨d, d∨d∨d∨ d♢d ∨ d♢d ∨ (d∨d)♢(d∨d)]
                      ,[_0,    _0,            _1, d∨d∨d                             ]
                      ,[_0,    _0,            _0,              _1                      ]
                      ]-}

```

To get specs to work we need a notion of matrix dimension.
Note that so far we are only interested in square matrices.

Additional tests planned:

* behaviour of a `Matrix Int`
* `Matrix Earliest` behaves like shortest distance
* `Matrix LatencyDistribution` converges
