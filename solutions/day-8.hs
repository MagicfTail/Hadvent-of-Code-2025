module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import GHC.Float (int2Float)
import Data.List (tails, sortBy)
import Data.PSQueue  qualified as Psq
import Data.DisjointSet qualified as Dsj

type Coord    = (Int, Int, Int)
type Input    = [Coord]
type Solution = Int

parseLine :: Atto.Parser (Int, Int, Int)
parseLine = (,,) <$> Atto.decimal <* Atto.char ',' <*> Atto.decimal <* Atto.char ',' <*> Atto.decimal

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parseLine `Atto.sepBy` Atto.endOfLine <* Atto.skipSpace) rawData of
    Left err  -> error err
    Right res -> res

distance :: Coord -> Coord -> Float
distance (x1, y1, z1) (x2, y2, z2) = sqrt (int2Float (x2 - x1)^(2 :: Int) + int2Float (y2 - y1)^(2 :: Int) + int2Float (z2 - z1)^(2 :: Int))

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

takeTop :: (Ord p, Ord k) => Int -> Psq.PSQ k p -> [Psq.Binding k p]
takeTop 0 _ = []
takeTop n q = case Psq.minView q of
    Nothing          -> []
    Just (binding, rest) -> binding : takeTop (n - 1) rest

takeAll :: (Ord p, Ord k) => Psq.PSQ k p -> [Psq.Binding k p]
takeAll q = case Psq.minView q of
    Nothing          -> []
    Just (binding, rest) -> binding : takeAll rest

joinUntilOne :: Ord a => [(a, a)] -> Dsj.DisjointSet a -> (a, a)
joinUntilOne [] _ = error "Disjoined set never got merged"
joinUntilOne ((coordA, coordB):xs) s
  | Dsj.sets newSet == 1 = (coordA, coordB)
  | otherwise = joinUntilOne xs newSet
      where
        newSet = Dsj.union coordA coordB s

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 input = product $ take 3 orderedSets where
  queue = foldl (\acc (pairA, pairB) -> Psq.insert (pairA, pairB) (distance pairA pairB) acc) Psq.empty $ pairs input
  coordPairs = map Psq.key $ takeTop 1000 queue

  sets = foldl (\acc (pairA, pairB) -> Dsj.union pairA pairB acc) Dsj.empty coordPairs

  orderedSets = map length $ sortBy (\a b -> length b `compare` length a) $ Dsj.toLists sets

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 input = uncurry (\(x1, _, _) (x2, _, _) -> x1 * x2) final where
  queue = foldl (\acc (pairA, pairB) -> Psq.insert (pairA, pairB) (distance pairA pairB) acc) Psq.empty $ pairs input
  coordPairs = map Psq.key $ takeAll queue

  completeSet = Dsj.fromLists $ map (: []) input

  final = joinUntilOne coordPairs completeSet


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

