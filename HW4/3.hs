sumQuanity :: Integer -> (Integer, Integer)

sumQuanity n | n == 0 = (0, 0)
  | n < 0 = sumQuanity (abs n)
  | otherwise = (mod n 10 + sum_, 1 + quanity) where
    (sum_, quanity) = sumQuanity (div n 10)
