{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Latency.ErlangSpec(spec, gamma, distributionSeries, exponential) where

import Control.Exception(assert)
import GHC.Exts(IsList(..))

import Probability
import Series
import Latency

import Test.QuickCheck
import Test.QuickCheck.All
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Statistics.Distribution.Gamma       as Statistics(gammaDistr)
import qualified Statistics.Distribution.Exponential as Statistics(exponential)
import qualified Statistics.Distribution             as Statistics(Distribution(..))

distributionSeries :: Statistics.Distribution d => d -> Int -> LatencyDistribution
distributionSeries distribution steps = LatencyDistribution . Series . (Prob <$>) . take steps
                                      $ assert (Statistics.cumulative distribution 0.0==0.0)
                                      $ ser 0 0.0
  where
    ser :: Int -> Double -> [Double]
    ser i curCum = nextCum - curCum:ser next nextCum
      where
        next    = i+1
        nextCum = Statistics.cumulative distribution
                $ fromIntegral next

gamma k theta = distributionSeries $ Statistics.gammaDistr k theta

exponential a = distributionSeries $ Statistics.exponential a

-- Create a series that contains n steps of integrals of `gamma` distribution for given arguments shape and scale
gamma' :: Double -> Double -> [Double]
gamma' k theta = assert (Statistics.cumulative distribution 0.0==0.0) $ ser 0 0.0
  where
    distribution = Statistics.gammaDistr k theta
    ser :: Int -> Double -> [Double]
    ser i curCum = nextCum - curCum:ser next nextCum
      where
        next    = i+1
        nextCum = Statistics.cumulative distribution
                $ fromIntegral next

oldGamma steps k theta = LatencyDistribution $ Series (Prob <$> take steps (gamma' k theta))

--erlang k lambda x = gamma k (lambda * x)/(k-1)

spec = do
  it "Truthy" $ property $ True `shouldBe` True
  it "Sum of two gammas is gamma with different shape parameter" $ property $
    \steps theta -> let g i = gamma steps i theta
                    in g 1 `firstToFinish` g 1 `shouldBe` g 2

