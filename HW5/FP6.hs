module FP6 where

import FP5

instance Show a => Show (SortedList a) where
  show = show . fromSortedList

singleton :: a -> SortedList a
singleton x = SortedList [x]

fromSortedList :: SortedList a -> [a]
fromSortedList (SortedList xs) = xs

fsthalf :: [a] -> [a]
fsthalf xs = take (length xs `div` 2) xs

sndhalf :: [a] -> [a]
sndhalf xs = drop (length xs `div` 2) xs

msort :: Ord a => [a] -> SortedList a
msort [] = mempty
msort [x] = singleton x
msort xs = (msort (fsthalf xs)) <> (msort (sndhalf xs))
