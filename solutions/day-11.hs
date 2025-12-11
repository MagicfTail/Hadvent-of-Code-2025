module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Array ( (!), array )
import Data.List (elemIndex)
import Data.Maybe (fromJust)

type In       = String
type Out      = String
type Device   = (In, [Out])
type Input    = [Device]
type Solution = Int

parseConnection :: Atto.Parser String
parseConnection = BS.unpack <$> Atto.take 3

parseOuts :: Atto.Parser [Out]
parseOuts = parseConnection `Atto.sepBy` Atto.char ' '

parseLine :: Atto.Parser Device
parseLine = (,) <$> parseConnection <* Atto.string ": " <*> parseOuts

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parseLine `Atto.sepBy` Atto.endOfLine) rawData of
    Left err  -> error err
    Right res -> res

getIndex :: (Eq a) => [a] -> a -> Int
getIndex elements element =  fromJust $ elemIndex element elements

countPathsFrom :: In -> Out -> Input -> Int
countPathsFrom start end input = memo!startIndex where
  inputWithOut = input ++ [("out", [])]

  inputs = map fst inputWithOut
  n = length inputs

  startIndex = getIndex inputs start
  endIndex = getIndex inputs end

  bounds = (0, n)

  memo = array bounds [(inVal, calc inVal) | inVal <- [0..n]]

  calc i
    | i == endIndex = 1
    -- Calling getIndex every time is very inefficient. The values should be mapped once before enterign the recursive function, however, I can't be bothered.
    | otherwise = sum $ map (\a -> memo!getIndex inputs a) $ snd $ inputWithOut!!i

countPathsFrom2 :: In -> Out -> Input -> Int
countPathsFrom2 start end input = memo!(startIndex, 0 :: Int, 0 :: Int) where
  inputWithOut = input ++ [("out", [])]
  
  inputs = map fst inputWithOut
  n = length inputs

  startIndex = getIndex inputs start
  endIndex = getIndex inputs end

  fftIndex = getIndex inputs "fft"
  dacIndex = getIndex inputs "dac"

  bounds = ((0, 0, 0), (n, 2, 2))

  memo = array bounds [((inVal, metFFT, metDAC), calc inVal metFFT metDAC) | inVal <- [0..n], metFFT <- [0..1], metDAC <- [0..1]]

  calc i metFFT metDAC
    | i == endIndex && (metFFT == 0 || metDAC == 0) = 0
    | i == endIndex && (metFFT == 1 && metDAC == 1) = 1
    | otherwise =  subValues where
        hasFFT = if metFFT == 1 || i == fftIndex then 1 else 0
        hasDAC = if metDAC == 1 || i == dacIndex then 1 else 0
        -- Calling getIndex every time is very inefficient. The values should be mapped once before enterign the recursive function, however, I can't be bothered.
        subValues = sum $ map (\a -> memo!(getIndex inputs a, hasFFT, hasDAC)) $ snd $ inputWithOut!!i

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = countPathsFrom "you" "out"

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = countPathsFrom2 "svr" "out"

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

