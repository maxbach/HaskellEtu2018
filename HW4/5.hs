f:: (a -> a) -> Int -> (a -> a)
f fun n
  | n < 1 = error "n must be positive number"
  | n == 1 = fun
  | otherwise = fun. f fun (n-1)
