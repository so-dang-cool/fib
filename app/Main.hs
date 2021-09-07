module Main where

import FibLib
import System.Environment

main :: IO ()
main = getArgs >>= runFibber
