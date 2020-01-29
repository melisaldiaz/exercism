{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Phone
  ( number
  ) where

import qualified Data.Char as Char

-- | Given a North American Numbering Plan (NANP)
-- telephone number, it validates it and cleans
-- up its format.
number :: String -> Maybe String
number xs =
  let pn :: String
      pn = filter Char.isDigit xs
   in case pn of
        [] -> Nothing
        _ ->
          case and (fmap Char.isDigit pn) of
            False -> Nothing
            True ->
              case valid11digitNum pn of
                Just v11 -> Just v11
                Nothing  -> valid10digitNum pn

-- | Given a number, it checks whether
-- it is a valid 10 digit NANP number.
valid10digitNum :: String -> Maybe String
valid10digitNum n =
  case n of
    [] -> Nothing
    _ ->
      case length n == 10 of
        False -> Nothing
        True  -> validN n

-- | Given a number, it checks whether
-- it is a valid 11 digit NANP number.
valid11digitNum :: String -> Maybe String
valid11digitNum n =
  case n of
    [] -> Nothing
    _ ->
      case length n == 11 of
        False -> Nothing
        True ->
          case n of
            x:xs ->
              if x == '1'
                then validN xs
                else Nothing
            _ -> Nothing

-- | Given a ten digit number, (NXX)-NXX-XXXX
-- it checks whether the N number is valid.
-- N can only be a digit between 2 and 9
validN :: String -> Maybe String
validN n =
  case n of
    a:_:_:d:_ ->
      let a1 = Char.ord a
          d1 = Char.ord d
       in if (a1 >= 50 && a1 <= 57) 
             && 
             (d1 >= 50 && d1 <= 57)
          then Just n
          else Nothing
    _ -> Nothing
