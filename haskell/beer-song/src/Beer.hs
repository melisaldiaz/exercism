module Beer
  ( song
  ) where

song :: String
song = lyrics 99

lyrics :: Int -> String
lyrics n =
  case n of
    0 ->
      "No more bottles of beer on the wall, no more bottles of beer.\n\
             \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    1 ->
      "1 bottle of beer on the wall, 1 bottle of beer.\n\
            \Take it down and pass it around, no more bottles of beer on the wall.\n\
            \\n" ++
      lyrics (n - 1)
    _ ->
      case line n of
        Nothing -> ""
        Just l  -> l ++ lyrics (n - 1)

line :: Int -> Maybe String
line n
  | n > 1 && n <= 99 =
    Just
      ((show n) ++
       " bottles of beer on the wall, " ++
       (show n) ++
       " bottles of beer.\n\
       \Take one down and pass it around, " ++
       if n - 1 == 1
         then "1 bottle of beer on the wall.\n\
       \\n"
         else (show (n - 1)) ++
              " bottles of beer on the wall.\n\
       \\n")
  | otherwise = Nothing
