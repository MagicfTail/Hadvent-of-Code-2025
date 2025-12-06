module Main where

import System.Environment (getArgs)
import Control.Applicative ((<|>))
import Data.List (transpose)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import Data.List.Split (splitWhen)
import Data.Attoparsec.ByteString.Char8 (isSpace)

type Input1    = ([[Int]], [Operator])
type Input2    = ([[Char]], [Operator])
type Solution = Int

data Operator = Mul | Add deriving (Show)

skipSpaces :: Atto.Parser ()
skipSpaces = Atto.skipWhile (== ' ')

parseNumberWithTrailingSpace :: Atto.Parser Int
parseNumberWithTrailingSpace = Atto.decimal <* skipSpaces

parseNumberLine :: Atto.Parser [Int]
parseNumberLine = skipSpaces *> Atto.many1 parseNumberWithTrailingSpace

parseOperator :: Atto.Parser Operator
parseOperator =
  (Mul <$ Atto.char '*')
  <|> (Add <$ Atto.char '+')

parseOperators :: Atto.Parser [Operator]
parseOperators = parseOperator `Atto.sepBy` skipSpaces

parseInput1 :: Atto.Parser Input1
parseInput1 = (,)
  <$> parseNumberLine `Atto.sepBy` Atto.endOfLine
  <*  Atto.endOfLine
  <*> parseOperators

parser1 :: B.ByteString -> Input1
parser1 rawData =
  case Atto.parseOnly parseInput1 rawData of
    Left err -> error err
    Right res -> res

parseCharLine :: Atto.Parser [Char]
parseCharLine = BS.unpack <$> Atto.takeWhile1 (\c -> c == ' ' || Atto.isDigit c)

parseInput2 :: Atto.Parser Input2
parseInput2 = (,)
  <$> parseCharLine `Atto.sepBy` Atto.endOfLine
  <*  Atto.endOfLine
  <*> parseOperators

parser2 :: B.ByteString -> Input2
parser2 rawData =
  case Atto.parseOnly parseInput2 rawData of
    Left err -> error err
    Right res -> res

applyOperator :: Operator -> [Int] -> Int
applyOperator operator values = case operator of
  Mul -> product values
  Add -> sum values

applyOperatorString :: Operator -> [String] -> Int
applyOperatorString operator values = case operator of
  Mul -> product $ map read values
  Add -> sum $ map read values

-- | The function which calculates the solution for part one
solve1 :: Input1 -> Solution
solve1 (values, operators) = sum . zipWith applyOperator operators $ transpose values

-- | The function which calculates the solution for part two
solve2 :: Input2 -> Solution
solve2 (chars, operators) = sum . zipWith applyOperatorString (reverse operators) $ modifiedInput where
  modifiedInput = splitWhen (all isSpace) . transpose $ map reverse chars

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  if read @Int part == 1
    then do
      input <- parser1 <$> B.readFile filepath -- use parser <$> readFile filepath if String is better
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      input <- parser2 <$> B.readFile filepath -- use parser <$> readFile filepath if String is better
      putStrLn "solution to problem 2 is:"
      print $ solve2 input

