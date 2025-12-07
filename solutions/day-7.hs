module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Control.Applicative ((<|>))
import Data.Array ( (!), array, assocs, bounds, indices, listArray, Array )
import Data.Graph ( graphFromEdges, reachable )
import Data.Maybe (fromJust)
import Data.List (find)

type Input    = Array (Int, Int) Position
type Solution = Int

data Position = Start | Free | Split deriving (Show, Eq)

parseLine :: Atto.Parser [Position]
parseLine = Atto.many1 $
      (Start <$ Atto.char 'S')
  <|> (Free <$ Atto.char '.')
  <|> (Split <$ Atto.char '^')

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parseLine `Atto.sepBy` Atto.endOfLine) rawData of
    Left err  -> error err
    Right res -> listToArray2D res

listToArray2D :: [[a]] -> Array (Int, Int) a
listToArray2D rows = listArray ((0, 0), (h - 1, w - 1)) (concat rows)
  where
    h = length rows
    w = if h > 0 then length (head rows) else 0

mapToGraphInput :: ((Int, Int), [(Int, Int)]) -> (String, (Int, Int), [(Int, Int)])
mapToGraphInput (a, b) = (show a, a, b)

generateVertexes :: Input -> [(Int, Int)] -> [(String, (Int, Int), [(Int, Int)])]
generateVertexes input splits = map mapToGraphInput values where
  (_, (height, _)) = bounds input

  memo = array (bounds input) [(pos, lookUp pos) | pos <- indices input]

  lookUp (y, x)
    | y == height = []
    | input!(y, x) == Split = [(y, x)]
    | otherwise = memo!(y+1, x)

  values = map (\(idy, idx) -> ((idy, idx), memo!(idy, idx - 1) ++ memo!(idy, idx + 1))) splits

recursiveCount :: Input -> (Int, Int) -> Int
recursiveCount input startPos = memo!startPos where
  (_, (height, _)) = bounds input

  memo = array (bounds input) [(pos, findPosibilities pos) | pos <- indices input]

  findPosibilities (y, x)
    | y == height = 1
    | input!(y, x) == Split = memo!(y, x - 1) + memo!(y, x + 1)
    | otherwise = memo!(y + 1, x)

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 input = length $ reachable graph entrance where
  splits = map fst $ filter ((== Split) . snd) $ assocs input

  (graph, _, vertexFromKey) = graphFromEdges $ generateVertexes input splits

  entrance = fromJust $ vertexFromKey $ head splits

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 input = recursiveCount input startPos where
  startPos = fst . fromJust . find ((== Start) . snd) $ assocs input

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> B.readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      -- putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input

