module FibLib ( runFibber ) where

import System.Exit
import Data.Char

-- CLI messages

programVersion :: String
programVersion = "fib v1"

pad :: String -> String
pad s = "  " ++ s

usage :: String
usage = unlines $ "USAGE:" : map pad ["fib", "fib <N>...", "fib --until N"]

docs :: String
docs = unlines
  [
    "The program fib generates numbers from the fibonacci sequence. When",
    "run without arguments, it reads lines of integers from standard",
    "input. For each integer N, the Nth member of the sequence is printed,",
    "separated by lines. Integers can also be provided as arguments to get",
    "the same (line-separated) output.",
    "",
    "When the --until flag is used, all members of the sequence are printed",
    "on separate lines, finishing with the Nth member of the sequence."
  ]

-- CLI message handlers

printHelp :: IO ()
printHelp = putStrLn $ unlines [programVersion, "", usage, docs]

usageError :: IO a
usageError = die usage

-- CLI runners

runFibber :: [String] -> IO ()
runFibber [] = stdinFibber
runFibber ["--help"] = printHelp
runFibber ["-h"] = printHelp
runFibber ["--until", n] = untilFibber n
runFibber args = argFibber args

stdinFibber :: IO ()
stdinFibber = interact (lineFibber "Line" . lines)

argFibber :: [String] -> IO ()
argFibber args = putStr . lineFibber "Argument" $ args

untilFibber :: String -> IO ()
untilFibber = putStr . sequenceFibber

-- Fibonacci generation

lineFibber :: String -> [String] -> String
lineFibber label = unlines . map fibber . zip [1..]
  where
    fibber :: (Int, String) -> String
    fibber (line, "") = "[ERROR] " ++ label ++ " " ++ show line ++ " is empty."
    fibber (line, num)
      | all isDigit num = fib . read $ num
      | otherwise = "[ERROR] " ++ label ++ " " ++ show line ++ " is not a non-negative integer."

sequenceFibber :: String -> String
sequenceFibber = unlines . fibSeq . read

fib :: Int -> String
fib n = show $ fibs !! n

fibSeq :: Int -> [String]
fibSeq n = map show . take (n + 1) $ fibs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

