{-|
Module      : FibCli
Description : A simple implementation of a Fibonacci sequence CLI.
Copyright   : Copyright (c) 2021 J.R. Hill
License     : BSD3
Maintainer  : justin@so.dang.cool
Stability   : Don't Panic

This module is not intended for use as a library for anything other
than its own CLI. Feel free to try it out though. Let me know if you
do something interesting with it.

For CLI usage instructions, run it with a '--help' flag.

Versioning for the project is NOT semantic versioning. Versions will
follow the Fibonacci sequence.

* 1
* 1.1
* 1.1.2
* 1.1.2.3
* 1.1.2.3.5

...and so on.
-}
module FibCli (runFibber, CommandLineArgs) where

import Data.Char ( isDigit )
import System.Exit ( die )
import qualified Data.Numbers.Fibonacci as FastFib
import qualified Algorithm.Gutenberg.Fibonaccis as PreCalcFib

-- |Arguments passed to the CLI.
type CommandLineArgs = [String]

-- CLI messages

programVersion :: String
programVersion = "fib v1.1.2"

pad :: String -> String
pad s = "  " ++ s

usage :: String
usage = unlines $ "USAGE:" : map pad ["fib", "fib <N>...", "fib --until N"]

docs :: String
docs =
  unlines
    [ "Calculate members of the Fibonacci sequence.",
      "",
      "Every N provided as an argument will return a line with the Nth member",
      "of the Fibonacci sequence. When no arguments are provided, the program",
      "instead reads lines from standard input. Arguments (or lines from",
      "standard input) should be non-negative integer values.",
      "",
      "This CLI considers a 0-indexed sequence, where the 0th number is 0,",
      "the 1st is 1, then 1, 2, 3, 5, 8...",
      "",
      "When '--until N' is used, all members of the sequence are printed on",
      "N+1 consecutive lines, including and ending with the Nth number of the",
      "sequence."
    ]

attribution :: String
attribution = "This project lives at https://github.com/booniepepper/fib"

-- CLI message handlers

printHelp :: IO ()
printHelp = putStr $ unlines [programVersion, "", usage, docs, attribution]

usageError :: IO a
usageError = die usage

-- CLI runners

-- |The 'runFibber' function runs a "fib" CLI.
runFibber :: CommandLineArgs -> IO ()
runFibber [] = stdinFibber
runFibber ["--help"] = printHelp
runFibber ["-h"] = printHelp
runFibber ["--until", n] = untilFibber n
runFibber ("--until":_) = usageError
runFibber args = argFibber args

-- Fibonacci at the IO level

stdinFibber :: IO ()
stdinFibber = interact (lineFibber "Line" . lines)

argFibber :: [String] -> IO ()
argFibber = putStr . lineFibber "Argument"

untilFibber :: String -> IO ()
untilFibber = putStr . sequenceFibber

-- Fibonacci at the String level

lineFibber :: String -> [String] -> String
lineFibber label = unlines . map fibber . zip [1 ..]
  where
    fibber :: (Int, String) -> String
    fibber (line, "") = "ERROR: " ++ label ++ " " ++ show line ++ " is empty."
    fibber (line, num)
      | all isDigit num = show . fibN . read $ num
      | otherwise = "ERROR: " ++ label ++ " " ++ show line ++ " is not a non-negative integer."

sequenceFibber :: String -> String
sequenceFibber = unlines . map show . (`take` fibs) . succ . read

-- Fibonacci at the [Integer] level

preCalculatedFibs :: [Integer]
preCalculatedFibs = PreCalcFib.first1001Fibs

fibN :: Int -> Integer
fibN 0 = 0
fibN n
  | n <= 1001 = preCalculatedFibs !! (n-1)
  | otherwise = FastFib.fib n

calculatedFibs :: [Integer]
calculatedFibs = a : b : zipWith (+) calculatedFibs (tail calculatedFibs)
  where
    a = fibN 1000
    b = fibN 1001

fibs :: [Integer]
fibs = 0 : preCalculatedFibs ++ drop 2 calculatedFibs
