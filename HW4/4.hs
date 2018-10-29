import           Data.Maybe

numTimesFound xs x = (length . filter (== x)) xs
first (a, _, _) = a

dominant list = checkDominant list (findDominantWrapper list)

checkDominant :: Eq a => [a] -> a -> Maybe(a)
checkDominant list element = if fromIntegral(numTimesFound list element) > ((fromIntegral $ length list) / 2)
  then Just (element)
  else Nothing

findDominantWrapper :: Eq a => [a] -> a
findDominantWrapper [] = error "List is empty"
findDominantWrapper xs = fromJust (first (findDominant (Nothing, 0, xs)))

findDominant :: (Eq a, Num b, Eq b) => (Maybe a, b, [a]) -> (Maybe a, b, [a])
findDominant (a, n, []) = (a, n, [])
findDominant (Nothing, _, (x:xs)) = findDominant (Just x, 1, xs)
findDominant (_, 0, (x:xs)) = findDominant (Just x, 1, xs)
findDominant (a, n, (x:xs)) = if ((fromJust a) == x)
  then findDominant (a, n+1, xs)
  else findDominant (a, n-1, xs)
