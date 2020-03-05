module Matrix
  ( saddlePoints
  ) where

import           Control.Monad (guard)
import           Data.Array
import           Data.Function (on)

saddlePoints :: Ord e => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix = do
  i <- indices matrix
  let row :: [(Int, Int)]
      row = filter ((on (==) snd) i) (indices matrix)
  let col :: [(Int, Int)]
      col = filter ((on (==) fst) i) (indices matrix)
  let rval = fmap (matrix !) row
  let cval = fmap (matrix !) col
  let val = matrix ! i
  guard (val == minimum rval)
  guard (val == maximum cval)
  return i
