module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString yW yB =
  let b0 = maybe emptyBoard (insertQueen 'W' (lines emptyBoard)) yW
   in maybe b0 (insertQueen 'B' (lines b0)) yB

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (ax, ay) (bx, by)
  | ax == bx = True
  | ay == by = True
  | otherwise = abs (ax - bx) == abs (ay - by)

insertQueen :: Char -> [String] -> (Int, Int) -> String
insertQueen queen board (x, y)
  | x > 7 || y > 7 = ""
  | otherwise =
    unlines $
      take x board
        ++ [replaceAt (y * 2) queen rank]
        ++ drop (x + 1) board

emptyBoard :: String
emptyBoard = unlines $ replicate 8 rank

rank :: String
rank = "_ _ _ _ _ _ _ _"

replaceAt :: Int -> Char -> String -> String
replaceAt _ _ "" = ""
replaceAt n c s
  | n >= length s = s
  | otherwise =
    case splitAt n s of
      (pre, post) -> pre ++ [c] ++ drop 1 post
