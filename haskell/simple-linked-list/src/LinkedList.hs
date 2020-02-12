module LinkedList
  ( LinkedList
  , datum
  , fromList
  , isNil
  , new
  , next
  , nil
  , reverseLinkedList
  , toList
  ) where

data LinkedList a
  = List
  | Elements a
             (LinkedList a)
  deriving (Eq, Show)

datum :: LinkedList a -> Maybe a
datum (Elements a _) = Just a
datum List           = Nothing

fromList :: [a] -> LinkedList a
fromList xs =
  case xs of
    a:rest -> Elements a (fromList rest)
    []     -> List

isNil :: LinkedList a -> Bool
isNil (Elements _ _) = False
isNil List           = True

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Elements x linkedList

next :: LinkedList a -> LinkedList a
next List            = List
next (Elements _ xs) = xs

nil :: LinkedList a
nil = List

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList =
  let f :: LinkedList a -> LinkedList a -> LinkedList a
      f ll acc =
        case ll of
          List          -> acc
          Elements x xs -> f xs (new x acc)
   in f linkedList List

toList :: LinkedList a -> [a]
toList linkedList =
  case linkedList of
    Elements x xs -> x : toList xs
    List          -> []
