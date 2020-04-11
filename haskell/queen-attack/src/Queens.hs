module Queens (boardString, canAttack) where

import Control.Monad (guard)

data Board = Board {unBoard :: String}
  deriving (Show)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString yW yB =
  case boardString' yW yB of
    Nothing -> error "invalid input"
    Just x -> unBoard x

boardString' :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe Board
boardString' yW yB = do
  b0 <- case yW of
    Nothing -> Just emptyBoard
    Just w -> insertQueen 'W' emptyBoard w
  case yB of
    Nothing -> Just b0
    Just b -> insertQueen 'B' b0 b

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (ax, ay) (bx, by)
  | ax == bx = True
  | ay == by = True
  | otherwise = abs (ax - bx) == abs (ay - by)

insertQueen :: Char -> Board -> (Int, Int) -> Maybe Board
insertQueen queen board (x, y) =
  boardFromLines $
    take x (boardToLines board)
      ++ [replaceAt (y * 2) queen rank]
      ++ drop (x + 1) (boardToLines board)

emptyBoard :: Board
emptyBoard = Board $ unlines $ replicate 8 rank

boardToLines :: Board -> [String]
boardToLines = lines . unBoard

boardFromLines :: [String] -> Maybe Board
boardFromLines x = do 
  guard (length x == 8)
  guard (all validRank x)
  pure (Board (unlines x))

validRank :: String -> Bool
validRank s
  | s == rank = True
  | otherwise = f s
  where
    f :: String -> Bool
    f x = case x of
      "" -> False
      a : [] -> a /= ' '
      a : b : rest ->
        if a /= ' ' && b == ' '
          then f rest
          else False

rank :: String
rank = "_ _ _ _ _ _ _ _"

replaceAt :: Int -> Char -> String -> String
replaceAt _ _ "" = ""
replaceAt n c s
  | n >= length s = s
  | otherwise =
    case splitAt n s of
      (pre, post) -> pre ++ [c] ++ drop 1 post
