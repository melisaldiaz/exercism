module School
  ( School(..)
  , add
  , empty
  , grade
  , sorted
  , Grade(..)
  , Student(..)
  ) where

import           Data.List       (sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String     (IsString (..))

newtype Grade =
  Grade Int
  deriving (Ord, Eq, Show)

newtype Student =
  Student String
  deriving (Ord, Eq, Show)

instance IsString Student where
  fromString s = Student s

newtype School =
  School (Map Grade [Student])

add :: Grade -> Student -> School -> School
add gradeNum student (School m) =
  let f :: [Student] -> [Student] -> [Student]
      f a b = sort (a ++ b)
   in School (Map.insertWith f gradeNum [student] m)

empty :: School
empty = School (Map.empty)

grade :: Grade -> School -> [Student]
grade gradeNum (School m) =
  case Map.lookup gradeNum m of
    Nothing -> []
    Just a  -> a

sorted :: School -> [(Grade, [Student])]
sorted (School m) = Map.toList m
