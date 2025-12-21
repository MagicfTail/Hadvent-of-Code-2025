module Main where

import System.Environment (getArgs)
import Control.Applicative ((<|>))
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import qualified Data.Vector as V
import Data.List (subsequences, transpose)
import Data.Ratio ((%), numerator)
import qualified Data.IntSet as IS
import Data.Matrix (Matrix (ncols, nrows), fromLists, toLists, rref, getCol, setSize, getRow, setElem, (!))
import Data.Maybe (fromJust)
import Data.Vector (Vector)


type Input    = [Machine]
type Solution = Int

type Button = [Int]
type Counts = V.Vector Int

type Machine = ([Bool], [Button], Counts)

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

parseWeights :: Atto.Parser Counts
parseWeights = do
    nums <- Atto.char '{' *> Atto.decimal `Atto.sepBy` Atto.char ',' <* Atto.char '}'
    return $ V.fromList nums

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
    finalActiveIndices = foldr toggle IS.empty indices
    toggle i set
      | i `IS.member` set = IS.delete i set
      | otherwise         = IS.insert i set
    in [ i `IS.member` finalActiveIndices | i <- [0..size-1] ]

checkEqual :: [Bool] -> [Bool] -> Bool
checkEqual listA listB = all (uncurry (==)) $ zip listA listB

solveMachine1 :: Machine -> Int
solveMachine1 (machine, buttons, _) = minimum $ map length correctSolutions where
  subs = subsequences buttons
  correctSolutions = filter (checkEqual machine . solveFast (length machine) . concat) subs

solve1 :: Input -> Solution
solve1 = sum . map solveMachine1

buildMatrix :: Machine -> (Matrix Rational, (Int, Int))
buildMatrix (_, buttons, countsVec) =
  let
    counts = V.toList countsVec
    numRows = length counts
    numCols = length buttons
    cols = map (toCol numRows) buttons ++ genMissing numRows (numRows - numCols) ++ [map fromIntegral counts]
  in
    (fromLists (transpose cols), (numRows, numCols))
  where
    toCol rows btn = [ if i `elem` btn then 1%1 else 0%1 | i <- [0..rows-1] ]
    genMissing :: Int -> Int -> [[Rational]]
    genMissing rows cols = [[0%1 | _ <- [0..rows-1]] | _ <- [0..cols-1]]


getFreeColls :: Matrix Rational -> [Int]
getFreeColls reducedMatrix = freecolls where
  nCols = ncols reducedMatrix
  fixedColls = map (\i -> fromJust . V.elemIndex 1 $ getRow i reducedMatrix ) [1..nrows reducedMatrix]

  freecolls = [
      val + 1 | val <- [0..nCols-1]
        , val `notElem` fixedColls
    ]

getNonZeroRowsForCol :: Matrix Rational -> Int -> Vector Int
getNonZeroRowsForCol reduxedMatrix col = V.map (+1) $ V.findIndices (/=0) $ getCol col reduxedMatrix

getRangeForFreeCol :: Matrix Rational -> Vector Rational -> Int -> [Integer]
getRangeForFreeCol reduxedMatrix requiredVals col = if maxVal == 0 then [] else [0..maxVal] where
  filledRows = getNonZeroRowsForCol reduxedMatrix col
  rowMaxes = V.map (\i -> requiredVals V.!(i-1) / reduxedMatrix!(i, col)) filledRows

  positiveRowMaxes = V.filter (>0) rowMaxes

  maxVal = if null positiveRowMaxes then 0 else ceiling $ V.maximum positiveRowMaxes

setIndex :: Matrix Rational -> Vector Rational -> Int -> Int -> Integer -> (Matrix Rational, Vector Rational)
setIndex reduxedMatrix requiredVals row col value = (newMatrix, newVector) where
  oldMatrixVal = reduxedMatrix!(row, col)
  newMatrix = setElem 0 (row, col) reduxedMatrix
  oldVectorVal = requiredVals V.! (row-1)
  newVector = V.unsafeUpd requiredVals [(row-1, oldVectorVal - (value%1 * oldMatrixVal))]

setFreeCol :: Matrix Rational -> Vector Rational -> Int -> Integer -> (Matrix Rational, Vector Rational)
setFreeCol reduxedMatrix requiredVals col value = (newMatrix, newVector) where
  filledRows = getNonZeroRowsForCol reduxedMatrix col

  (newMatrix, newVector) = V.foldl (\(m, v) row -> setIndex m v row col value) (reduxedMatrix, requiredVals) filledRows

isValidResult :: Vector Rational -> (Integer, Bool)
isValidResult vals = (numerator $ V.sum vals, V.all (>=0) vals)

applyRecursivly :: Matrix Rational -> Vector Rational -> [Int] -> (Integer, Bool)
applyRecursivly _ currentVals [] = isValidResult currentVals
applyRecursivly currentMatrix currentVals (freeCol : remaining) = result where
  colRange = getRangeForFreeCol currentMatrix currentVals freeCol

  allPossibilities = map (\i -> (setFreeCol currentMatrix currentVals freeCol i , i)) colRange

  allFurtherPossibilities = map (\((m, v), added) -> (applyRecursivly m v remaining, added)) allPossibilities
  allValidPossibilities = filter (snd . fst) allFurtherPossibilities
  validPossibilityCosts = map (\((cost, _), added) -> cost + added) allValidPossibilities

  result = if null validPossibilityCosts then (0, False) else (minimum validPossibilityCosts, True)

solveRREF :: Matrix Rational -> Vector Rational -> Integer
solveRREF reduxedMatrix requiredVals = best where
  freeColls = getFreeColls reduxedMatrix

  (best, _) = applyRecursivly reduxedMatrix requiredVals freeColls


solveMachine2 :: Machine -> Int
solveMachine2 machine = fromIntegral $ solveRREF minifiedReducedMat requiredValsAll where
    (mat, (_, matCols)) = buildMatrix machine
    reducedMat = case rref mat of
      Right val -> val
      Left err -> error err

    filteredReducedMat = fromLists $ filter (not . all (==0)) $ toLists reducedMat

    minifiedReducedMat = setSize 0 (nrows filteredReducedMat) matCols filteredReducedMat
    requiredValsAll = getCol (ncols filteredReducedMat) filteredReducedMat

solve2 :: Input -> Solution
solve2 = sum . map solveMachine2

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
