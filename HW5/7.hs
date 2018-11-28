import           Data.Monoid ((<>))

data Tree a = Nil | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
	foldMap f Nil = mempt         y
	foldMap f (Leaf x) = f x
	foldMap f (Node l k r) = (foldMap f l) <> (f k) <> (foldMap f r)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)

instance Foldable Preorder where
	foldMap f (PreO Nil) = mempty
	foldMap f (PreO (Leaf x)) = f x
	foldMap f (PreO (Node l k r)) = (f k) <> (foldMap f (PreO l)) <> (foldMap f (PreO r))

newtype Postorder a = PostO (Tree a) deriving (Eq, Show)

instance Foldable Postorder where
	foldMap f (PostO Nil) = mempty
	foldMap f (PostO (Leaf x)) = f x
	foldMap f (PostO (Node l k r)) = (foldMap f (PostO l)) <> (foldMap f (PostO r)) <> (f k)

newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable Levelorder where
	foldMap f a = mconcat (map f (step [a]))

step :: [Levelorder a] -> [a]
step [] = []
step ts = map element ts ++ step (concatMap subtrees ts)

element :: Levelorder a -> a
element (LevelO (Leaf x))     = x
element (LevelO (Node _ k _)) = k

subtrees :: Levelorder a -> [Levelorder a]
subtrees (LevelO Nil)              = []
subtrees (LevelO (Leaf _))         = []
subtrees (LevelO (Node Nil _ Nil)) = []
subtrees (LevelO (Node Nil _ r))   = [LevelO r]
subtrees (LevelO (Node l _ Nil))   = [LevelO l]
subtrees (LevelO (Node l _ r))     = [LevelO l, LevelO r]
