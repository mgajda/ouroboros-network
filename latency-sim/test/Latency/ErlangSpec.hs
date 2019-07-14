{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Latency.ErlangSpec(spec) where

import Control.Exception(assert)
import GHC.Exts(IsList(..))

import Latency

import Test.QuickCheck
import Test.QuickCheck.All
import Test.Hspec
import Test.Hspec.QuickCheck

import Statistics.Distribution.Gamma(gammaDistr)
import Statistics.Distribution(Distribution(..))

-- Create a series that contains n steps of integrals of `gamma` distribution for given arguments shape and scale
gamma' :: Double -> Double -> [Double]
gamma' k theta = assert (cumulative distribution 0.0==0.0) $ ser 0 0.0
  where
    distribution = gammaDistr k theta
    ser :: Int -> Double -> [Double]
    ser i curCum = nextCum - curCum:ser next nextCum
      where
        next    = i+1
        nextCum = cumulative distribution
                $ fromIntegral next

gamma steps k theta = LatencyDistribution $ Series (Prob <$> take steps (gamma' k theta))

--erlang k lambda x = gamma k (lambda * x)/(k-1)

spec = do
  it "Truthy" $ property $ True `shouldBe` True
  it "Sum of two gammas is gamma with different shape parameter" $ property $
    \steps theta -> let g i = gamma steps i theta
                    in g 1 `firstToFinish` g 1 `shouldBe` g 2
