lastFib :: Int -> Int
lastFib n = lastFibs !! n where
  lastFibs = 0 : 1 : next lastFibs where
    next (a : t@(b:_)) = (mod (a+b) 10) : next t
