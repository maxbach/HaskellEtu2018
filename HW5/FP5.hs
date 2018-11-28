module FP5 where

import Data.Monoid ((<>))

newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Ord)
instance Ord a => Monoid (SortedList a) where
  mempty = SortedList []
  mappend (SortedList xs) (SortedList ys) = SortedList $ mergeSortedLists xs ys

mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists xs [] = xs
mergeSortedLists [] ys = ys
mergeSortedLists (x:xs) (y:ys) = if x <= y
     then x : mergeSortedLists xs (y:ys)
     else y : mergeSortedLists (x:xs) ys
