module Main where

import Lib

main :: IO ()
main = interact $ unlines . map fibber . zip [1..] . lines

