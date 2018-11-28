module FP4 where

import Data.Ratio

seriesK :: Int -> [Rational]

seriesK n = map (\x -> 1 % ((toInteger n)^x)) [0, 1..]
