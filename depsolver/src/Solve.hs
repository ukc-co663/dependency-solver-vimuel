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

-- dependenciesAndConflicts :: Map Name [Pkg] -> Pkg -> [SmtCmd]
-- dependenciesAndConflicts repo p =
--   (map (smt_conflicts p) $ concatMap (get repo) $ confls p)
--                                   ++ (map (smt_depends p . concatMap (get repo)) . depends $ p)

dependencies :: (?repo :: Map Name [Pkg]) => Pkg -> [[Pkg]]
dependencies = map (concatMap (get ?repo)) . depends

conflicts :: (?repo :: Map Name [Pkg]) => Pkg -> [Pkg]
conflicts = concatMap (get ?repo) . confls


get :: Map Name [Pkg] -> PkgConstr -> [Pkg]
get repo (PkgConstr name versionConstr) =
  case Map.lookup name repo of
    Nothing -> []
    Just pkgs -> throwOut $ pkgs
  where
    throwOut = case versionConstr of
        Nothing -> id
        Just (r,v) -> filter $ \p -> toOperator r (version p) v

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
    hSetBuffering stdout NoBuffering
    counter <- newIORef (1 :: Int)
    (repo, initial, target) <- parseInput path

    let toZ3 = writeToPipe hZ3In counter path
        fromZ3 = do
          out <- Text.hGetLine hZ3Out
          when ?debugging $ Text.putStrLn out
          return out
        process :: (?repo :: Map Name [Pkg]) => Set Pkg -> [Pkg] -> IO (Set Pkg)
        process alreadyProcessed toProcess =
            case toProcess of
              [] -> return alreadyProcessed
              (p:toProcess) ->
                if p `Set.member` alreadyProcessed
                  then process alreadyProcessed toProcess
                  else do
                    toZ3 $ smt_declareConst p
                    if (pkgId p) `Set.member` initial
                      then toZ3 $ smt_inInitial p
                      else toZ3 $ smt_notInInitial p
                    let dss = dependencies p
                        cs = conflicts p
                    ps <- process (Set.insert p alreadyProcessed) ((concat dss) ++ cs ++ toProcess)
                    mapM_ toZ3 $ map (smt_depends p) dss
                    mapM_ toZ3 $ map (smt_conflicts p) cs
                    return ps

    putStr "Submitting constraints to Z3 ..."
    toZ3 =<< Text.readFile "src/smt-header.smt2"

    let ?repo = Map.fromListWith (++) . map (\(n,m) -> (n,[m])) $ repo
    allPkgs <- Set.toList <$> process Set.empty [target]

    toZ3 $ "(assert (installed " <> var target <> " t-final))"

    t2 <- getCurrentTime
    putStr "\rConstraints submitted to Z3 in "
    putStrLn . show $ diffUTCTime t2 t1

    let
      hone :: Int -> Bool -> IO Int
      hone n stop = do
          when (n > 10000) $ error "failed"
          putStr $ "Honing " ++ show n ++ "..."
          toZ3 "(push)"
          t <- getCurrentTime
          toZ3 $ "(assert (= t-final " <> (Text.pack . show) n <> "))"
          toZ3 "(check-sat)"
          res <- fromZ3
          t' <- getCurrentTime
          putStr $ "\rHoning " ++ show n ++ " took "
          putStrLn . show $ diffUTCTime t' t
          toZ3 "(pop)"
          case res of
            "sat" -> hone (n-1) True
            "unsat" -> if stop then return (n + 1) else if n == 0 then return 1 else hone (n*2) False
            x -> error $ show x
      -- hone :: Int -> Int -> Int -> IO Int
      -- hone comingFromBelow lower upper guess x =
      --   if lower == 0 || upper == 1 then error ""
      --     else case guess `compare` x of -- (assert ((>= t-final lower) (<= t-final upper))
      --         EQ -> []
      --         LT -> guess : if comingFromBelow then hone True (guess + 1) (upper * 2) (upper * 2) x
      --                       else hone False (guess + 1) upper ((upper + guess + 1) `div` 2) x
      --         GT -> guess : hone False lower (guess - 1) ((guess + lower - 1) `div` 2) x


    let t_final_lb = (length . depends) target + (length . confls) target

    -- t_final <- hone 1 False
    -- when ?debugging $ print t_final
    -- toZ3 $ "(assert (= t-final " <> (Text.pack . show) t_final <> "))"
    toZ3 "(check-sat)"
    "sat" <- fromZ3
    t3 <- getCurrentTime
    putStr "Found solution in "
    putStrLn . show $ diffUTCTime t3 t2

    toZ3 "(eval t-final)"
    t_final <- read . Text.unpack <$> fromZ3

    -- toZ3 "(eval t-final)"
    -- t_final <- read . Text.unpack <$> fromZ3
    -- when ?debugging $ putStrLn $ "t-final: " ++ show t_final

    matrix <- forM allPkgs $ \p -> do
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
