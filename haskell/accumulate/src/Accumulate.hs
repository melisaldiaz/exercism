module Accumulate
  ( accumulate
  ) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f xs =
  case xs of
    []     -> []
    a:rest -> f a : accumulate f rest
