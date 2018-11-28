import Data.Monoid ((<>))

newtype SortedList a = SortedList [a] deriving (Eq, Ord)
instance Ord a => Monoid (SortedList a) where
  mempty = SortedList []
  mappend (SortedList xs) (SortedList ys) = SortedList $ mergeSortedLists xs ys

instance Show a => Show (SortedList a) where
  show = show . fromSortedList

mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists xs [] = xs
mergeSortedLists [] ys = ys
mergeSortedLists (x:xs) (y:ys) = if x <= y
     then x : mergeSortedLists xs (y:ys)
     else y : mergeSortedLists (x:xs) ys

singleton :: a -> SortedList a
singleton x = SortedList [x]

fromSortedList :: SortedList a -> [a]
fromSortedList (SortedList xs) = xs

fsthalf :: [a] -> [a]
fsthalf xs = take (length xs `div` 2) xs

sndhalf :: [a] -> [a]
sndhalf xs = drop (length xs `div` 2) xs

mergesort :: Ord a => [a] -> SortedList a
mergesort [] = mempty
mergesort [x] = singleton x
mergesort xs = (mergesort (fsthalf xs)) <> (mergesort (sndhalf xs))
