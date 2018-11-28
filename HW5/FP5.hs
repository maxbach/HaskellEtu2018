module FP5 where

import Data.Semigroup (Semigroup (..))

newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Ord)

instance Ord a => Semigroup (SortedList a) where
  SortedList xs <> SortedList ys = SortedList $ mergeSortedLists xs ys
instance Ord a => Monoid (SortedList a) where
  mempty = SortedList []
  mappend = (<>)

mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists xs [] = xs
mergeSortedLists [] ys = ys
mergeSortedLists (x:xs) (y:ys) = if x <= y
     then x : mergeSortedLists xs (y:ys)
     else y : mergeSortedLists (x:xs) ys
