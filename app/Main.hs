{-# LANGUAGE ImplicitParams #-}
module Main where

import qualified Data.ByteString.Lazy as B (readFile)
import System.Environment (getArgs)

import Solve
-- import SolverUnsat

main :: IO ()
main = do
  -- (path:flags) <- getArgs
  -- let ?logging = "-l" `elem` flags
  --     ?debugging = "-d" `elem` flags
  -- doStuff path
  [repository,initial,constraints] <- getArgs
  putStrLn =<< doStuff repository initial constraints
