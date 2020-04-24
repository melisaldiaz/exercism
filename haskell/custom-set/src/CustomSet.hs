{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module CustomSet
  ( delete,
    difference,
    empty,
    fromList,
    insert,
    intersection,
    isDisjointFrom,
    isSubsetOf,
    member,
    null,
    size,
    toList,
    union,
  )
where

import Data.List (foldl')
import Prelude hiding (null)

data CustomSet a
  = Empty
  | Node (CustomSet a) a (CustomSet a)
  deriving (Show)

instance Eq a => Eq (CustomSet a) where 
  (==) a b = toList a == toList b 

delete :: Ord a => a -> CustomSet a -> CustomSet a
delete _ Empty = Empty
delete x (Node Empty a r) | x == a = r
delete x (Node l a r)
  | x < a = Node (delete x l) a r
  | x > a = Node l a (delete x r)
  | otherwise = case popMin r of
    Nothing -> l
    Just (m, r') -> Node l m r'

popMin :: Ord a => CustomSet a -> Maybe (a, CustomSet a)
popMin Empty = Nothing
popMin (Node l a r) = case popMin l of
  Nothing -> Just (a, r)
  Just (m, l') -> Just (m, Node l' a r)

-- Remove from setA the elements in setB
difference :: forall a. Ord a => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = foldr f setA (toList setB)
  where
    f :: a -> CustomSet a -> CustomSet a
    f x acc = delete x acc

empty :: CustomSet a
empty = Empty

fromList :: Ord a => [a] -> CustomSet a
fromList xs = foldl' (flip insert) Empty xs

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x Empty = Node Empty x Empty
insert x set@(Node l a r)
  | x == a = set
  | x < a = Node (insert x l) a r
  | otherwise = Node l a (insert x r)

intersection :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = fromList (filter (flip member setB) (toList setA))

isDisjointFrom :: Ord a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom Empty Empty = True
isDisjointFrom setA Empty = True
isDisjointFrom Empty setB = True
isDisjointFrom setA setB = not (any (flip member setB) (toList setA))

isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf Empty Empty = True
isSubsetOf setA Empty = False
isSubsetOf Empty setB = True
isSubsetOf setA setB
  | setA == setB = True
  | otherwise = all (flip member setB) (toList setA)

member :: Ord a => a -> CustomSet a -> Bool
member _ Empty = False
member x (Node l a r)
  | x == a = True
  | x < a = member x l
  | otherwise = member x r

null :: CustomSet a -> Bool
null Empty = True
null _ = False

size :: CustomSet a -> Int
size set = length (toList set)

toList :: CustomSet a -> [a]
toList Empty = []
toList (Node l a r) = (toList l) ++ [a] ++ (toList r)

union :: forall a. Ord a => CustomSet a -> CustomSet a -> CustomSet a
union setA Empty = setA
union Empty setB = setB
union setA setB = foldr insert setA (toList setB) 