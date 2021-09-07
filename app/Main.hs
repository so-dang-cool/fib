module Main where

import System.Environment
import FibLib

main :: IO ()
main = getArgs >>= runFibber

