module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Char qualified as Char
import Data.Array ( (!), array, listArray )

type Input    = [[Int]]
type Solution = Int

parseLine :: Atto.Parser [Int]
parseLine = Atto.many1 (Char.digitToInt <$> Atto.digit)

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parseLine `Atto.sepBy` Atto.endOfLine <* Atto.skipSpace) rawData of
    Left err  -> error err
    Right res -> res

getLargestPairFromLine :: [Int] -> Int
getLargestPairFromLine values
  | length values < 2 = 0
  | otherwise = maximum $ zipWith (\a b -> 10 * a + b) largestValuesRight largestValuesLeft where
      largestValuesRight = scanl1 max $ init values
      largestValuesLeft = reverse . scanl1 max . init $ reverse values


helper :: [Int] -> Int -> Int
helper numberList stepsMax = memo!(0, stepsMax) where
  n = length numberList
  bounds = ((0, 0), (n, stepsMax))

  numbers = listArray (0, n - 1) numberList

  memo = array bounds [((x, y), calc x y) | x <- [0..n], y <- [0..stepsMax]]

  calc _ 0 = 0
  calc position steps
    | length numbers - position == steps = numbers!position * 10^(steps - 1) + memo!(position + 1, steps - 1)
    | otherwise = max takeItem leaveItem where
      takeItem = numbers!position * 10^(steps - 1) + memo!(position + 1, steps - 1)
      leaveItem = memo!(position + 1, steps)
  

getLargestCollectionFromLine :: [Int] -> Int
getLargestCollectionFromLine numbers = helper numbers 12

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . map getLargestPairFromLine

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = sum . map getLargestCollectionFromLine

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

