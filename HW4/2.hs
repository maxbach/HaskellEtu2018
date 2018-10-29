import           Data.List

minMax :: Ord a => [a] -> Maybe (a, a)

minMax [] = Nothing
minMax list = Just(head sorted, last sorted) where sorted = sort list
