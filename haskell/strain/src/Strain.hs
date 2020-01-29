module Strain
  ( keep
  , discard
  ) where

keep :: (a -> Bool) -> [a] -> [a]
keep p xs =
  case xs of
    [] -> []
    a:rest ->
      case p a of
        True  -> a : keep p rest
        False -> keep p rest

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = keep (not . p) xs
