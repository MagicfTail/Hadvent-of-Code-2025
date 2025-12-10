module Main where

import System.Environment (getArgs)
import Control.Applicative ((<|>))
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import qualified Data.IntSet as Set
import Data.List (subsequences)

type Input    = [Machine]
type Solution = Int

type Button = [Int]
type Costs = [Int]

type Machine = ([Bool], [Button], Costs)

parseIndicators :: Atto.Parser [Bool]
parseIndicators = Atto.many1 $
      (True <$ Atto.char '#')
  <|> (False <$ Atto.char '.')

parseDiagram :: Atto.Parser [Bool]
parseDiagram = Atto.char '[' *> parseIndicators <* Atto.char ']'

parseButton :: Atto.Parser Button
parseButton = Atto.char '(' *> Atto.decimal `Atto.sepBy` Atto.char ',' <* Atto.char ')'

parseButtons :: Atto.Parser [Button]
parseButtons = parseButton `Atto.sepBy` Atto.char ' '

parseWeights :: Atto.Parser Costs
parseWeights = Atto.char '{' *> Atto.decimal `Atto.sepBy` Atto.char ',' <* Atto.char '}'

parseLine :: Atto.Parser Machine
parseLine = (,,)
  <$> parseDiagram
  <*  Atto.char ' '
  <*> parseButtons
  <*  Atto.char ' '
  <*> parseWeights

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parseLine `Atto.sepBy` Atto.endOfLine) rawData of
    Left err  -> error err
    Right res -> res

solveFast :: Int -> [Int] -> [Bool]
solveFast size indices =
  let
    finalActiveIndices = foldr toggle Set.empty indices

    toggle i set
      | i `Set.member` set = Set.delete i set -- Seen 2nd time (even): remove
      | otherwise          = Set.insert i set -- Seen 1st time (odd): add  
    in
      [ i `Set.member` finalActiveIndices | i <- [0..size-1] ]

checkEqual :: [Bool] -> [Bool] -> Bool
checkEqual listA listB = all (uncurry (==)) $ zip listA listB

solveMachine1 :: Machine -> Int
solveMachine1 (machine, buttons, _) = minimum $ map length correctSolutions where
  subs = subsequences buttons
  correctSolutions = filter (checkEqual machine . solveFast (length machine) . concat) subs

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . map solveMachine1

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

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

