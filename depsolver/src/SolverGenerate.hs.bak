{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
-- {-# LANGUAGE Strict #-}
module Solver where

import Control.Monad.IO.Class
import Data.Traversable (for, forM)

import qualified Data.Set as Set

import qualified Data.Map.Lazy as MapL

import Data.SBV
import Data.IORef

import Parser

import Debug.Trace

-- data SolverState = Solver
--   { varMap :: [((Name,Version), SBool)] }

go path = do
    repo <- repository path
    installed <- initial path
    target <- constraints path
    varRef <- newIORef MapL.empty
    memo <- newIORef MapL.empty
    satWith z3{verbose = False} $ do
        constraints <- mapM (mkConstraints repo installed memo Set.empty varRef) target
        return $ bAnd constraints

{-
; This example illustates extraction
; of unsatisfiable cores (a subset of assertions
; that are mutually unsatisfiable)
(set-option :produce-unsat-cores true)
(declare-fun p () Bool)
(declare-fun q () Bool)
(declare-fun r () Bool)
(declare-fun s () Bool)
; Z3 will only track assertions that are named.
(assert (! (or p q) :named a1))
(assert (! (implies r s) :named a2))
(assert (! (implies s (iff q r)) :named a3))
(assert (! (or r p) :named a4))
(assert (! (or r s) :named a5))
(assert (! (not (and r q)) :named a6))
(assert (! (not (and s p)) :named a7))
(check-sat)
(get-unsat-core)
-}

-- pass in list of dependencies already checked to stop cycles
mkConstraints :: Repo
  -> Set.Set RefString
  -> IORef (MapL.Map (Name,Version) Predicate)
  -> Set.Set (Name,Version)
  -> IORef (MapL.Map (Name,Version) SBool)
  -> Command
  -> IO ()
mkConstraints repo installed memoised alreadyUsed variables (Do action pkgRef) = do

  -- traceM $ toString (Do action pkgRef)

  -- look up the package in the repo to get all versions that satisfy the version predicate
  constraints <- case get repo pkgRef of

    -- if no package is found under that name, return empty constraints
    Nothing -> do
      -- traceM $ toString pkgRef ++ ": not found."
      
      -- return false

    -- a package of the name was found in the repo, note that pkgs may be []
    -- in which case the constraints will be empty as well
    Just (name, pkgs) ->

      -- go over every version of the package
      for pkgs $ \p -> do
        mem <- liftIO $ readIORef memoised
        case MapL.lookup (name, version p) mem of
          Just predic -> predic
          Nothing -> do

            -- lookup constraints in global state and return if already exists
            -- else mk constraints
            this <- do
              vs <- liftIO $ readIORef variables
              case MapL.lookup (name, version p) vs of
                Just v -> return v
                Nothing -> do
                  -- traceM $ (show $ ((name, version p, mkRef name p), vs)) ++ "\n\n"
                  fresh <- sBool $ mkRef name p
                  liftIO $ modifyIORef' variables (\vs -> MapL.insert (name, version p) fresh vs)
                  return fresh

            -- check whether we are adding or removing the package
            case action of

              -- if we are removing, then simply add a constraint saying that this
              -- package must not be installed
              Remove -> do
                traceM $ "Conflict, don't install " ++  mkRef name p
                return $ bnot this

              -- if we are adding, check for cyclic dependency (TODO: rethink this)
              Add ->
                if mkRefT name p `Set.member` installed then do
                -- if (name, version p) `Set.member` installed then do
                  traceM $ "Already installed: " ++ show (name, version p)
                  return this
                else if (name, version p) `Set.member` alreadyUsed then do
                  -- traceM $ show $ length alreadyUsed -- "cyclic dependency"
                  return $ bnot this -- add negative constraint because of cyclic dependency

                -- no cyclic dependency and not already installed, so carry on
                else do

                  -- go over clusters of disjuncts...
                  dependencies <- for (depends p) $ \disjs -> do

                    -- add constraint saying that at least one must be satisfied
                    depDisj <- for disjs (mkConstraints repo installed memoised (Set.insert (name, version p) alreadyUsed) variables . Do Add)
                    return $ bOr depDisj

                  -- go over all conflicts and require them to be removed (TODO: is this right way?)
                  conflicts <- for (conflicts p) (mkConstraints repo installed memoised alreadyUsed variables . Do Remove) -- add to already used?
                  let constraints = bAnd
                        [ this
                        , bAnd dependencies  -- if the current package, then all disjunctive dependencies
                        , bAnd conflicts     -- if the current package, then all the conflicts must be taken into account (TODO: think about this more carefully)
                        ]
                  liftIO $ modifyIORef' memoised (MapL.insert (name, version p) (return constraints))
                  return constraints

  --traceM $ "Finished making constraints for: " ++ toString (Do action pkgRef)
  -- finished looping over all matching versions of the package (if any)
  case constraints of

    -- no corresponding package or matching version found
    [] -> case action of
      Remove -> return true -- trivially true (TODO: is this right thing to do?)
      Add -> do
        -- traceM "No matching package"
        return false -- dependency can't be satisfied

    -- one or more packages found, and only one package has to be satisfied, so disjunction
    _  -> return $ bOr constraints



-- test = sat $ do
--   a <- sBool "A=2.01"
--   b <- sBool "B=3.0"
--   b' <- sBool "B=3.2"
--   c <- sBool "C=1"
--   d <- sBool "D=10.3.1"
--   solve
--       [ a
--       , bOr [b', c]
--       , d
--       , b' ==> bnot b
--       , c ==> bnot (bOr [b, b'])
--       , d ==> bnot b' ]

test = sat $ do
  a <- sBool "A=2.01"
  b <- sBool "B=3.0"
  b' <- sBool "B=3.2"
  c <- sBool "C=1"
  d <- sBool "D=10.3.1"
  solve
      [ a
      , bOr [b', c]
      , d
      , b' ==> bnot b
      , c ==> bnot (bOr [b, b'])
      , d ==> bnot b' ]
