module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.List (tails)
import Debug.Trace (traceShowId)

type Input    = [(Int, Int)]
type Solution = Int

data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Eq, Show)
data Turn = LeftTurn | RightTurn deriving (Eq, Show)

parseLine :: Atto.Parser (Int, Int)
parseLine = (,) <$> Atto.decimal <* Atto.char ',' <*> Atto.decimal

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parseLine `Atto.sepBy` Atto.endOfLine <* Atto.skipSpace) rawData of
    Left err  -> error err
    Right res -> res

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (+ 1) (max x1 x2 - min x1 x2) * (+ 1) (max y1 y2 - min y1 y2)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

mapToTriples :: [(Int, Int)] -> [((Int, Int), (Int, Int), (Int, Int))]
mapToTriples (x1 : x2 : xs) = helper (x1 : x2 : xs) where
  helper [] = []
  helper [h1] = [(h1, x1, x2)]
  helper [h1, h2] = [(h1, h2, x1), (h2, x1, x2)]
  helper values = (head values, values!!1, values!!2) : helper (tail values)
mapToTriples _ = error "Not enough args"

getDirection :: (Int, Int) -> (Int, Int) -> Direction
getDirection (fromX, fromY) (toX, toY)
  | fromX == toX && fromY > toY = UpDir
  | fromX == toX && fromY < toY = DownDir
  | fromY == toY && fromX > toX = LeftDir
  | fromY == toY && fromX < toX = RightDir
  | otherwise                   = error "Can't get direction of equal points"

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = maximum . map (uncurry area) . pairs

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 input = maximum areas where
  triples = mapToTriples input

  areas = traceShowId $ map (\(a, _, b) -> area a b) triples

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

