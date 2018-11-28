import Data.Ratio

seriesK :: Integer -> [Rational]

seriesK n = map (\x -> 1 % (n^x)) [0, 1..]
