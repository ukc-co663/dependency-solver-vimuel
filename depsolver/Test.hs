#!/usr/bin/env stack
{- stack script
   --resolver nightly-2018-01-21
   --install-ghc
   --package process
   --package text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import System.Process
import System.IO
import Data.Text as Text
import qualified Data.Text.IO as Text

main = do
  let info = Text.putStrLn
  info "Launching ghci."

  (Just hIn, Just hOut, _, p) <-
    createProcess (proc "ghci" ["-v0"])
      { std_in  = CreatePipe
      , std_out = CreatePipe
      , std_err = Inherit }
  hSetBuffering hIn NoBuffering
  hSetBuffering hOut NoBuffering
  let toP x = info x >> Text.hPutStrLn hIn x
      fromP = Text.hGetLine hOut

  toP "2 + 2"
  answer <- fromP
  case answer of
    "4" -> info "Woohoo!"
    _ -> info "Boohoo"
  info answer
  toP "2 + 2"
  answer <- fromP
  info answer
  hClose hIn
  hClose hOut
