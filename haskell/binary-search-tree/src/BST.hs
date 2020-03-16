module BST
  ( BST
  , bstLeft
  , bstRight
  , bstValue
  , empty
  , fromList
  , insert
  , singleton
  , toList
  ) where

import           Data.Foldable (foldl')

data BST a
  = Empty
  | Node (BST a)
         a
         (BST a)
  deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty        = Nothing
bstLeft (Node l _ _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Empty        = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Empty        = Nothing
bstValue (Node _ a _) = Just a

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList xs = foldl' (flip insert) Empty xs

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node l a r)
  | x <= a = Node (insert x l) a r
  | otherwise = Node l a (insert x r)

singleton :: a -> BST a
singleton x = Node Empty x Empty

toList :: BST a -> [a]
toList Empty        = []
toList (Node l a r) = (toList l) ++ [a] ++ (toList r)

instance Functor BST where
  fmap _ Empty        = Empty
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)
