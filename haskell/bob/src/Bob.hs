{-# LANGUAGE LambdaCase #-}

module Bob
  ( responseFor
  ) where

import           Data.Char     (isDigit, isPunctuation, isSpace, isSymbol,
                                isUpper)
import           Data.Foldable (foldl')

data S
  = Blank
  | Question
  | YelledQuestion
  | Yell
  | Other
  | Numbers

responseFor :: String -> String
responseFor xs = renderS (foldl' (flip update) Blank xs)

renderS :: S -> String
renderS =
  \case
    Blank -> "Fine. Be that way!"
    Question -> "Sure."
    YelledQuestion -> "Calm down, I know what I'm doing!"
    Yell -> "Whoa, chill out!"
    Other -> "Whatever."
    Numbers -> "Whatever."

update :: Char -> S -> S
update c =
  \case
    Blank -> updateBlank c
    Question -> updateQuestion c
    YelledQuestion -> updateYelledQuestion c
    Yell -> updateYell c
    Other -> updateOther c
    Numbers -> updateNumbers c

updateBlank :: Char -> S
updateBlank c
  | isSpace c = Blank
  | isUpper c = Yell
  | isDigit c = Numbers
  | otherwise = Other

updateQuestion :: Char -> S
updateQuestion c
  | isSpace c = Question
  | otherwise = Other

updateYelledQuestion :: Char -> S
updateYelledQuestion c
  | isSpace c = YelledQuestion
  | isUpper c || isPunctuation c || isDigit c = Yell
  | otherwise = Other

updateYell :: Char -> S
updateYell '?' = YelledQuestion
updateYell c
  | isSpace c || isUpper c || isPunctuation c || isDigit c || isSymbol c = Yell
  | otherwise = Other

updateOther :: Char -> S
updateOther '?' = Question
updateOther _   = Other

updateNumbers :: Char -> S
updateNumbers '?' = Question
updateNumbers c
  | isSpace c || isDigit c || isPunctuation c = Numbers
  | isUpper c = Yell
  | otherwise = Other
