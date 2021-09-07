module Main where

import FibCli
import System.Environment

main :: IO ()
main = getArgs >>= runFibber
