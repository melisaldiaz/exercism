{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module OCR
  ( convert,
    example,
  )
where

import Data.List (intercalate)

data Mark
  = Blank
  | Pipe
  | Underscore
  deriving (Show, Eq)

data Number
  = None
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Show, Eq, Ord)

convert :: String -> String
convert xs
  | correctSize xs == False =
    error "invalid input size."
  | length (lines xs) > 4 =
    intercalate "," $ fmap f $ multipleLines xs
  | otherwise = f xs
  where
    f :: String -> String
    f =
      concat
        . fmap renderNumber
        . fmap getNum
        . toListsOfMarks

multipleLines :: String -> [String]
multipleLines xs =
  fmap unlines (chunksOf 4 (lines xs))

correctSize :: String -> Bool
correctSize s = mod (length s) 4 == 0

renderNumber :: Number -> String
renderNumber =
  \case
    Zero -> "0"
    One -> "1"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    None -> "?"

getNum :: [Mark] -> Number
getNum list
  | list == zero = Zero
  | list == one = One
  | list == two = Two
  | list == three = Three
  | list == four = Four
  | list == five = Five
  | list == six = Six
  | list == seven = Seven
  | list == eight = Eight
  | list == nine = Nine
  | otherwise = None

toListsOfMarks :: String -> [[Mark]]
toListsOfMarks input =
  let [a, b, c, d] = (lines input)
      chunks :: [[(Char, Char, Char, Char)]]
      chunks = chunksOf 3 (zip4 a b c d)
      ls :: [[String]]
      ls = fmap toLines chunks
   in fmap (flip toMarks []) ls

renderMark :: Mark -> Char
renderMark =
  \case
    Blank -> ' '
    Pipe -> '|'
    Underscore -> '_'

-- Determines whether there's a pipe, an underscore
-- or a white space at a given index.
charAt :: Int -> String -> Maybe Mark
charAt i s
  | i >= length s = Nothing
  | i < 0 = Nothing
  | otherwise =
    if s !! i == renderMark Pipe
      then Just Pipe
      else
        if s !! i == renderMark Underscore
          then Just Underscore
          else
            if s !! i == renderMark Blank
              then Just Blank
              else Nothing

toMark :: String -> [Mark]
toMark s =
  case sequence $
    [charAt 0 s]
      ++ [charAt 1 s]
      ++ [charAt 2 s]
      ++ [charAt 3 s] of
    Nothing -> []
    Just ms -> ms

toMarks :: [String] -> [Mark] -> [Mark]
toMarks [] acc = acc
toMarks (x : xs) acc = toMarks xs (acc ++ toMark x)

toLines :: [(Char, Char, Char, Char)] -> [String]
toLines [] = []
toLines xs = foldr f [] xs
  where
    f :: (Char, Char, Char, Char) -> [String] -> [String]
    f t z = [[fst' t] ++ [snd' t] ++ [trd t] ++ [frth t]] ++ z

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 la lb lc ld =
  case (la, lb, lc, ld) of
    (a : as, b : bs, c : cs, d : ds) ->
      (a, b, c, d) : zip4 as bs cs ds
    (_, _, _, _) ->
      []

fst' :: (a, b, c, d) -> a
fst' (a, _, _, _) = a

snd' :: (a, b, c, d) -> b
snd' (_, b, _, _) = b

trd :: (a, b, c, d) -> c
trd (_, _, c, _) = c

frth :: (a, b, c, d) -> d
frth (_, _, _, d) = d

chunksOf :: forall a. Int -> [a] -> [[a]]
chunksOf n list =
  let f :: [a] -> [[a]]
      f x =
        case splitAt n x of
          (a, b)
            | null a -> []
            | otherwise -> a : (f b)
   in f list

-- Number patterns
zero, one, two, three, four, five, six, seven, eight, nine :: [Mark]
zero = [Blank, Pipe, Pipe, Blank, Underscore, Blank, Underscore, Blank, Blank, Pipe, Pipe, Blank]
one = [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Pipe, Pipe, Blank]
two = [Blank, Blank, Pipe, Blank, Underscore, Underscore, Underscore, Blank, Blank, Pipe, Blank, Blank]
three = [Blank, Blank, Blank, Blank, Underscore, Underscore, Underscore, Blank, Blank, Pipe, Pipe, Blank]
four = [Blank, Pipe, Blank, Blank, Blank, Underscore, Blank, Blank, Blank, Pipe, Pipe, Blank]
five = [Blank, Pipe, Blank, Blank, Underscore, Underscore, Underscore, Blank, Blank, Blank, Pipe, Blank]
six = [Blank, Pipe, Pipe, Blank, Underscore, Underscore, Underscore, Blank, Blank, Blank, Pipe, Blank]
seven = [Blank, Blank, Blank, Blank, Underscore, Blank, Blank, Blank, Blank, Pipe, Pipe, Blank]
eight = [Blank, Pipe, Pipe, Blank, Underscore, Underscore, Underscore, Blank, Blank, Pipe, Pipe, Blank]
nine = [Blank, Pipe, Blank, Blank, Underscore, Underscore, Underscore, Blank, Blank, Pipe, Pipe, Blank]
