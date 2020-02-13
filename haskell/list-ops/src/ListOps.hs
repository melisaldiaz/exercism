{-# LANGUAGE BangPatterns #-}
  
module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import           Prelude hiding (concat, filter, foldr, length, map, reverse,
                          (++))

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f !z (x:xs) = foldl' f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

length :: [a] -> Int
length xs =
  let f :: [a] -> Int -> Int
      f [] n       = n
      f (_:rest) n = f rest (n + 1)
   in f xs 0

reverse :: [a] -> [a]
reverse xs =
  let f :: [a] -> [a] -> [a]
      f [] l2       = l2
      f (a:rest) l2 = f rest (a : l2)
   in f xs []

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) =
  if p x
    then x : filter p xs
    else filter p xs

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs
