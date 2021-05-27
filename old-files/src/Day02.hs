module Day02 where

import Data.Function ((&))
import qualified Data.Text as T
-- import qualified Text.Regex.Posix as Regex

data Password =
  Password { min :: Int
           , max :: Int
           , letter :: Char
           , string :: String }
  deriving (Eq, Show)

parsePassword s =
  s
  & T.pack
  & T.strip
  & T.splitOn (T.pack " ")
  & (\l -> case l of
             [minMax, char, string] -> Just "foo"
             _ -> Nothing)
