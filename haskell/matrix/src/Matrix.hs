{-# LANGUAGE ScopedTypeVariables #-}

module Matrix
  ( Matrix
  , cols
  , column
  , flatten
  , fromList
  , fromString
  , reshape
  , row
  , rows
  , shape
  , transpose
  ) where

import           Control.Monad
import qualified Data.Attoparsec.Combinator as AC
import           Data.Attoparsec.Text       (Parser)
import qualified Data.Attoparsec.Text       as A
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V

data Matrix a =
  Matrix (Vector (Vector a))
  deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix m) =
  case m V.!? 0 of
    Nothing -> 0
    Just x  -> V.length x

column :: Int -> Matrix a -> Vector a
column x (Matrix m) = fmap (V.! (x - 1)) m

flatten :: Matrix a -> Vector a
flatten (Matrix m) = join m

fromList :: [[a]] -> Matrix a
fromList xss = Matrix (V.fromList (fmap V.fromList xss))

fromString :: String -> Matrix Int
fromString xs =
  case A.parseOnly parseMatrix (T.pack xs) of
    Left _  -> error "Cannot parse Matrix."
    Right x -> x

reshape :: forall a. (Int, Int) -> Matrix a -> Matrix a
reshape (_, cl) matrix =
  let va :: Vector a
      va = flatten matrix
   in Matrix (chunksOfV cl va)

row :: Int -> Matrix a -> Vector a
row x (Matrix m) = (V.!) m (x - 1)

rows :: Matrix a -> Int
rows (Matrix m) = V.length m

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose mx =
  let n :: Int
      n = cols mx
   in Matrix (V.fromList (fmap (flip column mx) [1 .. n]))

parseRow :: Parser (Vector Int)
parseRow = do
  r <- AC.sepBy A.decimal (A.char ' ')
  case null r of
    True  -> fail "Empty row."
    False -> pure (V.fromList r)

parseMatrix :: Parser (Matrix Int)
parseMatrix = do
  m <- AC.sepBy parseRow (A.char '\n')
  pure (Matrix (V.fromList m))

chunksOfV :: Int -> Vector a -> Vector (Vector a)
chunksOfV n v =
  let f :: Vector a -> Vector (Vector a)
      f va =
        case V.splitAt n va of
          (a, b)
            | null a -> V.empty
            | otherwise -> V.cons a (f b)
   in f v
