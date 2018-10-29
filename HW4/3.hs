sumQuanity :: Integer -> (Integer, Integer)

sumQuanity 0 = (0, 0)
sumQuanity n = (mod n 10 + sum_, 1 + quanity) where
  (sum_, quanity) = sumQuanity (div n 10)
