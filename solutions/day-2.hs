module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Attoparsec.ByteString.Char8 (isDigit)
import Data.List (find)
import Data.Maybe (fromMaybe)

type Input    = [Range]
type Solution = Int

type Range = (String, String)

parseRange :: Atto.Parser Range
parseRange = (,)
  <$> ( B.unpack <$> Atto.takeWhile1 isDigit <* Atto.char '-')
  <*> (B.unpack <$> Atto.takeWhile1 isDigit)

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parseRange `Atto.sepBy` Atto.char ',') rawData of
    Left err -> error err
    Right res -> res

isRepeating :: Int -> Bool
isRepeating i
  | odd (length stringI) = False
  | otherwise = do
    let (lString, rString) = splitAt (length stringI `div` 2) stringI
    lString == rString
    where
      stringI = show i

countRange :: Range -> Int
countRange i = do
  let (intL, intR) = (read (fst i) :: Int, read (snd i) :: Int)
  sum (map (\val -> if isRepeating val then val else 0) [intL..intR])

isRepeatingFor :: Int -> Int -> Bool
isRepeatingFor i count = 
  let stringI = show i
      (lString, rString) = splitAt count stringI
      lLength = length lString
      rLength = length rString
  in rLength /= 0 && ((rLength `mod` lLength) == 0) && (concat (replicate (rLength `div` lLength) lString) == rString)

countAllRanges :: Range -> Int
countAllRanges i = do
  let (intL, intR) = (read (fst i) :: Int, read (snd i) :: Int)
  sum (map inner [intL..intR])
  where
    inner item =
      let stringItem = show item
      in fromMaybe 0 (find (/= 0) (map (\count -> if isRepeatingFor item count then item else 0) [1..length stringItem `div` 2]))

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 input = sum (map countRange input)

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 input = sum (map countAllRanges input)

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

