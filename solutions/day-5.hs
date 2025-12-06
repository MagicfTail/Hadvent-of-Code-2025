module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.List (sort)

type Input    = ([Range], [Int])
type Solution = Int

type Range = (Int, Int)

parseRange :: Atto.Parser Range
parseRange = (,) <$> Atto.decimal <* Atto.char '-' <*> Atto.decimal

parseInput :: Atto.Parser Input
parseInput = (,)
  <$> parseRange `Atto.sepBy` Atto.endOfLine
  <*  Atto.count 2 Atto.endOfLine
  <*> Atto.decimal `Atto.sepBy` Atto.endOfLine

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly parseInput rawData of
    Left err -> error err
    Right res -> res

(<?<) :: Ord a => a -> (a,a) -> Bool
(<?<) x (start, end) = start <= x && x <= end

inAnyRange :: [Range] -> Int -> Bool
inAnyRange ranges number = any (number <?<) ranges

mergeRanges :: [Range] -> [Range]
mergeRanges ((lo1, hi1) : (lo2, hi2) : rest)
  | lo2 <= hi1 = mergeRanges ((lo1, max hi1 hi2) : rest)
  | otherwise = (lo1, hi1) : mergeRanges ((lo2, hi2) : rest)
mergeRanges [x] = [x]
mergeRanges [] = []

rangeSize :: Range -> Int
rangeSize (lo, hi) = hi - lo + 1

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 (ranges, ids) = length $ filter (inAnyRange ranges) ids

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 (ranges, _) = sum . map rangeSize . mergeRanges $ sort ranges

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
