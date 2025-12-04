module Main where

import System.Environment (getArgs)
import Data.ByteString qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Control.Applicative ((<|>))
import Data.Array (Array, Ix, inRange, bounds, (!), array, indices, listArray, elems)

type Input    = [[Spot]]
type Solution = Int

data Spot = Paper | Free deriving (Show, Eq)

parseLine :: Atto.Parser [Spot]
parseLine = Atto.many1 $
      (Free <$ Atto.char '.')
  <|> (Paper <$ Atto.char '@')

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parseLine `Atto.sepBy` Atto.endOfLine <* Atto.skipSpace) rawData of
    Left err  -> error err
    Right res -> res

(!?) :: (Ix i) => Array i e -> i -> e -> e
(!?) arr index def =
    if inRange (bounds arr) index
    then arr!index
    else def

mapWithIndex :: (Ix i) => (i -> a -> b) -> Array i a -> Array i b
mapWithIndex f arr = array (bounds arr)
    [ (i, f i (arr ! i)) | i <- indices arr ]

countAdjacent :: Array (Int, Int) Spot -> (Int, Int) -> Int
countAdjacent grid (px, py) = length
    [ () | dx <- [-1 .. 1], dy <- [-1 .. 1]
         , (dx, dy) /= (0, 0)
         , let neighbor = (px + dx, py + dy)
         , (grid !? neighbor $ Free) == Paper
    ]

listToArray2D :: [[a]] -> Array (Int, Int) a
listToArray2D rows = listArray ((0, 0), (h - 1, w - 1)) (concat rows)
  where
    h = length rows
    w = if h > 0 then length (head rows) else 0

replacePaper :: Array (Int, Int) Spot -> Array (Int, Int) Spot
replacePaper old = array (bounds old)
  [
    (pos, newState)
    | pos <- indices old
    , let newState = if countAdjacent old pos < 4 then Free else old!pos
  ]

runUntilNoChange :: (Array (Int, Int) Spot -> Array (Int, Int) Spot) -> Array (Int, Int) Spot -> Array (Int, Int) Spot
runUntilNoChange f input =
  let nextGrid = f input
  in if input == nextGrid
    then input
    else runUntilNoChange f nextGrid


-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 input = length
    [
      () | pos <- indices inputArray
      , inputArray!pos == Paper
      , countAdjacent inputArray pos < 4
    ]
    where
      inputArray = listToArray2D input

-- | The function which calculates the solution for part two
solve2 :: Input -> Int
solve2 input = foldl (\acc (old, new) -> acc + if old /= new then 1 else 0) 0 $ zip (elems inputArray) (elems finalArray)
  where
    inputArray = listToArray2D input
    finalArray = runUntilNoChange replacePaper inputArray

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

