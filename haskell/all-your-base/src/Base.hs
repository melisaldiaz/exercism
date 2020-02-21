{-# LANGUAGE ScopedTypeVariables #-}

module Base
  ( Error(..)
  , rebase
  ) where

data Error a
  = InvalidInputBase
  | InvalidOutputBase
  | InvalidDigit a
  deriving (Show, Eq)

rebase ::
     forall a. Integral a
  => a
  -> a
  -> [a]
  -> Either (Error a) [a]
rebase inputBase outputBase inputDigits =
  case getError inputBase outputBase inputDigits of
    Just e -> Left e
    Nothing -> Right (f (toDecimal inputBase inputDigits) [])
      where f :: a -> [a] -> [a]
            f n acc =
              case n of
                0 -> acc
                _ -> f (div n outputBase) (rem n outputBase : acc)

getError :: Integral a => a -> a -> [a] -> Maybe (Error a)
getError inputBase outputBase digits
  | inputBase <= 1 = Just InvalidInputBase
  | outputBase <= 1 = Just InvalidOutputBase
  | any (\x -> x < 0) digits = Just (InvalidDigit (minimum digits))
  | any (\x -> x >= inputBase) digits = Just (InvalidDigit (maximum digits))
  | otherwise = Nothing

-- Given a number, represented as a list of digits, and
-- the base in which it is, it converts it to base-10.
toDecimal ::
     forall a. Integral a
  => a
  -> [a]
  -> a
toDecimal inputBase digits = sum (fmap f (indexedList digits))
  where
    f :: (a, a) -> a
    f a = snd a * inputBase ^ fst a

-- Returns a list of the digits in reverse order with their
-- corresponding index.
-- example: [3,5] -> [(0,5), (1,3)]
indexedList :: Integral a => [a] -> [(a, a)]
indexedList digits = zip [0 ..] (reverse digits)
