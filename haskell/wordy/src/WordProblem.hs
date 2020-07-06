{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module WordProblem
  ( answer,
  )
where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.List (foldl')
import qualified Data.Text as T
import Data.Text (Text)

data Operation
  = Addition
  | Subtraction
  | Multiplication
  | Division
  deriving (Show)

data MathExp
  = MathNum Integer
  | MathOp MathExp Operation Integer
  deriving (Show)

answer :: String -> Maybe Integer
answer problem =
  case A.parseOnly (parseExp <* A.endOfInput) (T.pack problem) of
    Left _ -> Nothing
    Right p -> Just (getResult p)

-- Given a MathExp'ression, return the correct answer.
-- Operations associate to the left.
getResult :: MathExp -> Integer
getResult m =
  case m of
    MathNum a -> a
    MathOp a op b -> (toOperator op) (getResult a) b

-- Parser for mathematical problems.
parseExp :: Parser MathExp
parseExp = do
  _ <- A.string "What is "
  a :: MathExp <- parseMathNum
  xs :: [(Operation, Integer)] <- A.many' $ do
    _ <- A.char ' '
    o <- parseOperation
    _ <- A.char ' '
    b <- parseNum
    pure (o, b)
  _ <- A.char '?'
  let f :: MathExp -> (Operation, Integer) -> MathExp
      f acc (op, num) = MathOp acc op num
  pure (foldl' f a xs)

-- Parser for MathNums.
parseMathNum :: Parser MathExp
parseMathNum = fmap MathNum parseNum

-- Parser for negative or positive decimals.
parseNum :: Parser Integer
parseNum = A.signed A.decimal

-- Given an Operation, return the correct operator.
toOperator :: Operation -> Integer -> Integer -> Integer
toOperator =
  \case
    Addition -> (+)
    Subtraction -> (-)
    Multiplication -> (*)
    Division -> div

-- Given a Text, return the corresponding Operation, if any.
toOperation :: Text -> Maybe Operation
toOperation =
  \case
    "plus" -> Just Addition
    "minus" -> Just Subtraction
    "multiplied by" -> Just Multiplication
    "divided by" -> Just Division
    _ -> Nothing

-- Parser for Operations.
parseOperation :: Parser Operation
parseOperation = do
  t <- A.choice $ fmap A.string operationWords
  case toOperation t of
    Nothing -> fail "Not a valid operation."
    Just op -> pure op

-- Supported operation words.
operationWords :: [Text]
operationWords =
  [ "plus",
    "minus",
    "multiplied by",
    "divided by"]
