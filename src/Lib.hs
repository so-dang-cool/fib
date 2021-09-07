module Lib ( fibber ) where

import Data.Char

fibber :: (Int, String) -> String
fibber (line, num)
  | all isDigit num = fib . read $ num
  | otherwise = "Line " ++ show line ++ ": Not a positive, nonzero integer."

fib :: Int -> String
fib n = go 1 1 n
  where
    go :: Integer -> Integer -> Int -> String
    go a b 0 = show a
    go a b n = go b (a+b) (n-1)


-- Sequence (space-separated)
-- TODO: Add some flags to choose presentation style
-- fib :: Int -> String
-- fib n = unwords . map show . take n $ fibs
-- fibs :: [Integer]
-- fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

