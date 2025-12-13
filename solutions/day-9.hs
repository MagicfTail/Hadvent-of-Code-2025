module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.List (tails, sortBy, find)

type Point    = (Int, Int)
type Edge     = (Point, Point)
type Input    = [Point]
type Solution = Int

parseLine :: Atto.Parser Point
parseLine = (,) <$> Atto.decimal <* Atto.char ',' <*> Atto.decimal

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parseLine `Atto.sepBy` Atto.endOfLine <* Atto.skipSpace) rawData of
    Left err  -> error err
    Right res -> res

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

inRange :: Int -> (Int, Int) -> Bool
inRange i (start, end) = start <= i && i <= end

inRangeSharp :: Int -> (Int, Int) -> Bool
inRangeSharp i (start, end) = start < i && i < end

pointsToEdges :: [Point] -> [Edge]
pointsToEdges [] = []
pointsToEdges [_] = []
pointsToEdges points = zip points (tail $ cycle points)

corners :: Point -> Point -> (Point, Point, Point, Point)
corners (aX, aY) (bX, bY) = ((minX, minY), (maxX, minY), (maxX, maxY), (minX, maxY)) where
  minX = min aX bX
  maxX = max aX bX
  minY = min aY bY
  maxY = max aY bY

pointOnEdge :: Point -> Edge -> Bool
pointOnEdge (pointX, pointY) ((edgeStartX, edgeStartY), (edgeEndX, edgeEndY)) = onVerticalEdge || onHorizontalEdge where
  minX = min edgeStartX edgeEndX
  maxX = max edgeStartX edgeEndX
  minY = min edgeStartY edgeEndY
  maxY = max edgeStartY edgeEndY

  isVertical = edgeStartX == edgeEndX

  onVerticalEdge = isVertical && pointX == edgeStartX && inRange pointY (minY, maxY)
  onHorizontalEdge = not isVertical && pointY == edgeStartY && inRange pointX (minX, maxX)

pointInsideShape :: [Edge] -> Point -> Bool
pointInsideShape edges point =  (== (1 :: Integer)) (edgesToRight `mod` 2) where
  edgesToRight = countEdgesToRight edges point

  countEdgesToRight [] _ = 0
  countEdgesToRight (((edgeStartX, edgeStartY), (edgeEndX, edgeEndY)) : remainders) (pointX, pointY) = countEdgesToRight remainders (pointX, pointY) + edgeHit where
    isVertical = edgeStartX == edgeEndX

    minY = min edgeStartY edgeEndY
    maxY = max edgeStartY edgeEndY

    edgeHit = if isVertical && pointX < edgeStartX && minY <= pointY && pointY < maxY then 1 else 0

pointInShape :: [Edge] -> Point -> Bool
pointInShape edges point = inside || onEdge where
  inside = pointInsideShape edges point
  onEdge = any (pointOnEdge point) edges

edgesBetweenPoints :: Point -> Point -> [Edge] -> Bool
edgesBetweenPoints (aX, aY) (bX, bY) = edgeBetweenPoints where
  pointMinX = min aX bX
  pointMaxX = max aX bX
  pointMinY = min aY bY
  pointMaxY = max aY bY

  isVerticalSeg = aX == bX

  edgeBetweenPoints [] = False
  edgeBetweenPoints (((edgeStartX, edgeStartY), (edgeEndX, edgeEndY)) : remainder) = val where
    edgeMinX = min edgeStartX edgeEndX
    edgeMaxX = max edgeStartX edgeEndX
    edgeMinY = min edgeStartY edgeEndY
    edgeMaxY = max edgeStartY edgeEndY

    isVerticalEdge = edgeStartX == edgeEndX

    val
      | isVerticalSeg && not isVerticalEdge && inRangeSharp edgeStartY (pointMinY, pointMaxY) && inRangeSharp aX (edgeMinX, edgeMaxX) = True
      | not isVerticalSeg && isVerticalEdge && inRangeSharp edgeStartX (pointMinX, pointMaxX) && inRangeSharp aY (edgeMinY, edgeMaxY) = True
      | otherwise = edgeBetweenPoints remainder

validSquare :: [Edge] -> (Point, Point) -> Bool
validSquare edges (pointA, pointB) = allInside && not anyIntersectingEdges where
  (tl, tr, br, bl) = corners pointA pointB

  allInside = pointInShape edges tl && pointInShape edges tr && pointInShape edges br && pointInShape edges bl

  anyIntersectingEdges = edgesBetweenPoints tl tr edges || edgesBetweenPoints tr br edges || edgesBetweenPoints br bl edges || edgesBetweenPoints bl tl edges

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = maximum . map (uncurry area) . pairs

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 input = area p1 p2 where
  edges = pointsToEdges input

  pointPairs = pairs input
  sortedPointPairs = sortBy (\(a1, a2) (b1, b2) -> area b1 b2 `compare` area a1 a2) pointPairs

  point = find (validSquare edges) sortedPointPairs

  (p1, p2) = case point of
    Just p -> p
    Nothing -> error "No solution found"

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
