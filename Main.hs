module Main where

import qualified System.IO as SIO
import qualified Day01 as D01
import Data.Function ((&))

main = do
  line <- SIO.readFile "data/day01.txt"
  return (line & D01.transformInput)
