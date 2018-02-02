module Main where

import qualified Data.ByteString.Lazy as B (readFile)
import System.Environment (getArgs)

import SolverUnsat

main :: IO ()
main = do
  (path:_) <- getArgs
  print =<< go path
