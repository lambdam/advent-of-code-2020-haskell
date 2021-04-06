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

input' = do
  line <- SIO.readFile "data/day01.txt"
  return (line & transformInput)

resultPart1 = do
  input <- input'
  [ (x, y) | x <- input, y <- input, x + y == 2020 ]
    & L.head
    & uncurry (*)
    & return

resultPart2 = do
  input <- input'
  [ (x, y, z) | x <- input, y <- input, z <- input, x + y + z == 2020 ]
    & L.head
    & \(x, y, z) -> x * y * z
    & return
