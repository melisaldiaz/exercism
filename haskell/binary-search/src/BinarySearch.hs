{-# LANGUAGE ScopedTypeVariables #-}

module BinarySearch
  ( find
  ) where

import           Data.Array

find ::
     forall a. Ord a
  => Array Int a
  -> a
  -> Maybe Int
find haystack needle
  | null haystack = Nothing

  | needle == mdEl = Just mdIx

  | needle < mdEl =
    let fstHalf :: Array Int a
        fstHalf = 
          listArray 
            (mn, mdIx - 1) 
            (take (mdIx - mn) els)
     in find fstHalf needle

  | otherwise =
    let sndHalf :: Array Int a
        sndHalf = 
          listArray 
            (mdIx + 1, mx) 
            (drop (mdIx - mn + 1) els)
     in find sndHalf needle
  where
    els :: [a]
    els = elems haystack

    (mn, mx) = bounds haystack

    mdIx :: Int
    mdIx = div (mn + mx) 2
    
    mdEl :: a
    mdEl = haystack ! mdIx