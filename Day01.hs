module Day01 where

import qualified System.IO as SIO
import qualified Data.Text as T
import qualified Data.List as L
import Data.Function ((&))
import Control.Arrow ((>>>))

transformInput :: String -> [Int]
transformInput s =
  s
  & T.pack
  & T.splitOn (T.pack "\n")
  & L.map T.unpack
  & L.filter (null >>> not)
  & L.map read

input' :: IO [Int]
input' = do
  line <- SIO.readFile "data/day01.txt"
  return (line & transformInput)

--- Part 1

resultPart1 :: [Int] -> Int
resultPart1 input =
  [ (x, y) | x <- input, y <- input, x + y == 2020 ]
    & L.head
    & uncurry (*)

getResultPart1 :: IO Int
getResultPart1 = do
  input <- input'
  return (resultPart1 input)

--- Part 2

resultPart2 :: [Int] -> Int
resultPart2 input =
  [ (x, y, z) | x <- input, y <- input, z <- input, x + y + z == 2020 ]
    & L.head
    & \(x, y, z) -> x * y * z

getResultPart2 :: IO Int
getResultPart2 = do
  input <- input'
  return (resultPart2 input)
