{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module ETL
  ( transform
  ) where


import qualified Data.Text as Text
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Char       as Char
import Data.Foldable (foldl')

transform :: Map a String -> Map Char a
transform legacyData = 
    foldl' Map.union 
        Map.empty
        (fmap (uncurry tag) (Map.toList legacyData))


tag :: a -> String -> Map Char a
tag n string = 
    let s :: String
        s = concat 
            (fmap Text.unpack 
                    (Text.chunksOf 1 
                      (Text.pack string)
                    )
            )
    in 
    Map.fromList 
     (fmap (\x -> (Char.toLower x, n)) 
     s)
