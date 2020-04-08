{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Brackets
  ( arePaired,
  )
where

data Side
  = Opening
  | Closing

data Bracket
  = RoundBracket
  | CurlyBracket
  | SquareBracket
  deriving (Eq)

arePaired :: String -> Bool
arePaired xs =
  arePaired' (getBrackets xs) []

arePaired' :: String -> [Bracket] -> Bool
arePaired' [] (_ : _) = False
arePaired' [] [] = True
arePaired' (a : rest) [] =
  case bracketType a of
    Nothing -> False
    Just (Closing, _) -> False
    Just (Opening, b) -> arePaired' rest [b]
arePaired' (a : rest) acc@(b : bs) =
  case bracketType a of
    Nothing -> False
    Just (Opening, x) -> arePaired' rest (x : acc)
    Just (Closing, x) ->
      case b == x of
        False -> False
        True -> arePaired' rest bs

bracketType :: Char -> Maybe (Side, Bracket)
bracketType =
  \case
    '[' -> Just (Opening, SquareBracket)
    ']' -> Just (Closing, SquareBracket)
    '(' -> Just (Opening, RoundBracket)
    ')' -> Just (Closing, RoundBracket)
    '{' -> Just (Opening, CurlyBracket)
    '}' -> Just (Closing, CurlyBracket)
    _ -> Nothing

getBrackets :: String -> String
getBrackets s = filter isBracket s

isBracket :: Char -> Bool
isBracket =
  \case
    '(' -> True
    ')' -> True
    '{' -> True
    '}' -> True
    '[' -> True
    ']' -> True
    _ -> False
