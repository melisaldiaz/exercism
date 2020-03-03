{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module House
  ( rhyme
  ) where

import           Data.List (intercalate)

rhyme :: String
rhyme = 
  intercalate "\n" (scanl getRhyme firstStanza whoWhat)

firstStanza :: String
firstStanza = 
  "This is the house that Jack built.\n"

prefix :: String
prefix = "This is "

whoWhat :: [(String, String)]
whoWhat =
  [ ("the malt\n", "that lay in ")
  , ("the rat\n", "that ate ")
  , ("the cat\n", "that killed ")
  , ("the dog\n", "that worried ")
  , ("the cow with the crumpled horn\n", "that tossed ")
  , ("the maiden all forlorn\n", "that milked ")
  , ("the man all tattered and torn\n", "that kissed ")
  , ("the priest all shaven and shorn\n", "that married ")
  , ("the rooster that crowed in the morn\n", "that woke ")
  , ("the farmer sowing his corn\n", "that kept ")
  , ("the horse and the hound and the horn\n", "that belonged to ")
  ]

getRhyme :: String -> (String, String) -> String
getRhyme stanza (who, what) =
  prefix ++ who ++ what ++ drop (length prefix) stanza
