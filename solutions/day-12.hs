module Main where

import System.Environment (getArgs)
import Data.ByteString qualified as B
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)

type Region   = (Int, Int, [Int])
type Input    = [Region]
type Solution = Int

parseEntries :: Atto.Parser [Int]
parseEntries = Atto.decimal `Atto.sepBy` Atto.char ' '

parseLine :: Atto.Parser Region
parseLine = (,,)
  <$> Atto.decimal
  <*  Atto.char 'x'
  <*> Atto.decimal
  <*  Atto.string ": "
  <*> parseEntries

-- New helper: Try to parse a Region, otherwise consume garbage until end of line
parseMaybeRegion :: Atto.Parser (Maybe Region)
parseMaybeRegion = 
      (Just <$> Atto.try parseLine) 
  <|> (Nothing <$ Atto.skipWhile (\c -> c /= '\n' && c /= '\r'))

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (catMaybes <$> parseMaybeRegion `Atto.sepBy` Atto.endOfLine) rawData of
    Left err  -> error err
    Right res -> res

minimumBoxes :: Int -> Int -> Int
minimumBoxes width height = (width `div` 3) * (height `div` 3)

checkRegion :: Region -> Bool
checkRegion (width, height, quantities) = sum quantities <= minimumBoxes width height

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = length . filter checkRegion

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 is a christmas miracle, and doesn't exist"

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> B.readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input

