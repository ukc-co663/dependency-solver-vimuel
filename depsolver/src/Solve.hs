{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Solve where

import Data.Monoid
import Data.Traversable (for)
import Data.Ord (comparing)
import qualified Data.Foldable as Foldable (foldl', length)
import Data.List (sortBy, reverse, transpose)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, mapMaybe, isNothing, fromJust)
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Control.Monad (unless, forM, forM_, when)
import Data.IORef
import System.FilePath ((</>))
import System.Process
import Data.List (sortBy, subsequences)
import Data.Ord (comparing)
import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Read (decimal, signed)
import Data.Ord (Down(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Parser

type SmtCmd = Text.Text

var p              = "|" <> toText p <> "|"
listOf []          = " nil"
listOf (p:ps)      = "(insert " <> var p <> listOf ps <> ")"
smt_declareConst p = "(declare-const " <> var p <> " Pkg)"
smt_inInitial p    = "(assert (installed " <> var p <> " 0))"
smt_notInInitial p = "(assert (not (installed " <> var p <> " 0)))"
smt_depends p ps   = "(assert (depends " <> var p <> " " <> listOf ps <> "))"
smt_conflicts p p' = "(assert (conflicts " <> var p <> " " <> var p' <> "))"

dependenciesAndConflicts :: Map Name [Pkg] -> Pkg -> [SmtCmd]
dependenciesAndConflicts repo p = (map (smt_conflicts p) $ concatMap (get repo) $ conflicts p)
                                  ++ (map (smt_depends p . concatMap (get repo)) . depends $ p)

get :: Map Name [Pkg] -> PkgConstr -> [Pkg]
get repo (PkgConstr name versionConstr) =
  case Map.lookup name repo of
    Nothing -> []
    Just pkgs -> throwOut $ pkgs
  where
    throwOut = case versionConstr of
        Nothing -> id
        Just (r,v) -> filter $ \p -> toOperator r (version p) v

-- -- incredibly wasteful way to calculate this, but my time is more precious than CPU cycles
-- process :: Map Name [Pkg] -> [Pkg] -> Set Pkg -> [Pkg] -> [Pkg]
-- process repo toProcess alreadyProcessed constrs =
--   case toProcess of
--     [] -> constrs
--     (p:toProcess) ->
--       if Set.member p alreadyProcessed
--         then process repo toProcess alreadyProcessed constrs
--         else
--           let c = mkDepConfConstrs repo p
--           in process repo (depsConfls repo p ++ toProcess) (Set.insert p alreadyProcessed) (c:constrs)

doStuff path = do
    t1 <- getCurrentTime
    (Just hZ3In, Just hZ3Out, _, z3) <- createProcess (proc "z3" ["-smt2","-in"])
      -- createProcess (proc "cvc4" ["--lang","smt2"])
      -- createProcess (shell "time z3 -smt2 -in")
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit }
    hSetBuffering hZ3In NoBuffering
    hSetBuffering hZ3Out NoBuffering
    hSetBuffering stdin NoBuffering
    counter <- newIORef (1 :: Int)
    (repo, initial, target) <- parseInput path

    let repoMap = Map.fromListWith (++) . map (\(n,m) -> (n,[m])) $ repo
        toZ3 = writeToPipe hZ3In counter path
        fromZ3 = do
          out <- Text.hGetLine hZ3Out
          when ?debugging $ Text.putStrLn out
          return out
        allPkgs = target : map snd repo

    header <- Text.readFile "src/smt-header.smt2"
    toZ3 header

    -- declare all the constants in the repo
    -- define which ones are in the initial state and not
    -- TODO only declare reachable ones
    forM_ allPkgs $ \p -> do
      toZ3 $ smt_declareConst p
      if (pkgId p) `elem` initial
        then toZ3 $ smt_inInitial p
        else toZ3 $ smt_notInInitial p

    forM_ allPkgs $ \p -> do
      mapM_ toZ3 $ dependenciesAndConflicts repoMap p

    toZ3 $ "(assert (installed " <> var target <> " t-final))"

    let
      hone :: Int -> Bool -> IO Int
      hone n stop = do
          when (n > 100) $ error "failed"
          toZ3 "(push)"
          toZ3 $ "(assert (= t-final " <> (Text.pack . show) n <> "))"
          toZ3 "(check-sat)"
          toZ3 "(pop)"
          fromZ3 >>= \case
            "sat" -> hone (n-1) True
            "unsat" -> if stop then return (n + 1) else if n == 0 then return 1 else hone (n*2) False
            x -> error $ show x

    t2 <- getCurrentTime
    putStr "Constraints submitted to Z3 in "
    putStrLn . show $ diffUTCTime t2 t1

    t_final <- hone 1 False
    when ?debugging $ print t_final
    toZ3 $ "(assert (= t-final " <> (Text.pack . show) t_final <> "))"
    toZ3 "(check-sat)"
    "sat" <- fromZ3
    matrix :: [(Pkg,[Text.Text])] <- forM allPkgs $ \p -> do
      transitions <- forM [0..t_final] $ \t -> do
        toZ3 $ "(eval (installed " <> var p <> " " <> (Text.pack . show) t <> "))"
        fromZ3
      return (p,transitions)

    let result = map fst . sortBy (comparing snd) . concatMap toCmd $ matrix

    writeFile (path </> "commands.json") (show result)

    callProcess "python" $ ["tests/judge.py"]
                      ++ map (path </>)
                          [ "repository.json"
                          , "initial.json"
                          , "commands.json"
                          , "constraints.json" ]

toCmd :: (Pkg,[Text.Text]) -> [(Text.Text,Int)]
toCmd a@(p,_) = case snd (pkgId p) of [] -> []; _ -> toCmd' 1 a
toCmd' _ (_,[]) = []
toCmd' _ (_,[_]) = []
toCmd' n (p,("false":"true":xs)) = ("+" <> toText (pkgId p),n) : toCmd' (n+1) (p,("true":xs))
toCmd' n (p,("true":"false":xs)) = ("-" <> toText (pkgId p),n) : toCmd' (n+1) (p,("false":xs))
toCmd' n (p,(_:x':xs)) = toCmd' (n+1) (p,(x':xs))

writeToPipe hIn counter path cmd = do
  when ?logging $ do
    i <- readIORef counter
    -- Text.appendFile (path </> (filter (/= '/') path <> ".smt2")) $ cmd <> " ; " <> Text.pack (show i) <> "\n"
    Text.appendFile "log.txt" $ cmd <> " ; " <> Text.pack (show i) <> "\n"
    modifyIORef' counter (+1)
  Text.hPutStrLn hIn cmd
