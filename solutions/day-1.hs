module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Control.Applicative ((<|>))

type Input    = [Int]
type Solution = Int

parserLine :: Atto.Parser Int
parserLine =
      (negate <$> (Atto.string "L" *> Atto.decimal))
  <|> (Atto.string "R" *> Atto.decimal)

parser :: B.ByteString -> Input
parser rawData =
  case Atto.parseOnly (parserLine `Atto.sepBy` Atto.endOfLine <* Atto.skipSpace) rawData of
    Left err  -> error err
    Right res -> res

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = length . filter (== 0) . scanl (\acc i -> (acc + i) `mod` 100) 50

-- | The function which calculates the solution for part two
solve2 :: Input -> Int
solve2 = snd . foldl step (50, 0)
  where
    step (position, clicks) rotation =
      let newPos = (position + rotation) `mod` 100
          newClicks = div (abs rotation) 100
           + if (position /= 0 && newPos /= 0) && ((rotation > 0 && newPos < position) || (rotation < 0 && newPos > position)) then 1 else 0
           + if newPos == 0 then 1 else 0
      in (newPos, clicks + newClicks)
      


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
