module FP2 where

indices :: [a] -> [(Int, a)]
zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]

indices list = zip [0..] list
zeroBy list fun = map (zeroMap fun) list
zeroMap fun x = if (fun x) then x else mempty
triplewiseSum a b c = map sum3 (zip3 a b c)
sum3 (a, b, c) = a + b + c
