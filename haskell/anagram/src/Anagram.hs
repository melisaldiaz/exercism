module Anagram
  ( anagramsFor
  ) where

import qualified Data.MultiSet as MS
import           Data.Text     (Text, toLower, unpack)

anagramsFor :: Text -> [Text] -> [Text]
anagramsFor xs xss =
  let tb :: [(Text, Bool)]
      tb = (zip xss (fmap (isAnagram xs) xss))
   in foldr (\t l -> fst t : l) [] (filter (\t -> snd t) tb)

-- Takes a word and a candidate, and determines
-- whether the second is an anagram of the first.
isAnagram :: Text -> Text -> Bool
isAnagram word candidate =
  if w /= c
    then (MS.toOccurList (MS.fromList (unpack w))) ==
         (MS.toOccurList (MS.fromList (unpack c)))
    else False
  where
    w = toLower word
    c = toLower candidate
