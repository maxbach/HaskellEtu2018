circShiftL :: Int -> [a] -> [a]
circShiftL n [] = []
circShiftL n list
  | n == 0 = list
  | length list == 0  = []
  | n > 0 = circShiftL (n-1) ((tail list) ++ ((head list):[]))
  | otherwise = circShiftL (n+1) (((last list):[]) ++ (init list))
