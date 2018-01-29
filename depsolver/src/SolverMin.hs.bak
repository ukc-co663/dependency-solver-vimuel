{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
-- {-# LANGUAGE Strict #-}
module SolverMin where

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
    target <- constraints path
    varRef <- newIORef MapL.empty
    memo <- newIORef MapL.empty
    satWith z3{verbose = False} $ do
        constraints <- mapM (mkConstraints repo memo Set.empty varRef) target
        return $ bAnd constraints


-- pass in list of dependencies already checked to stop cycles
mkConstraints :: Repo
  -> IORef (MapL.Map (Name,Version) Predicate)
  -> Set.Set (Name,Version)
  -> IORef (MapL.Map (Name,Version) SBool)
  -> Command
  -> Predicate
mkConstraints repo memoised alreadyUsed variables (Do action pkgRef) = do

  -- traceM $ toString (Do action pkgRef)

  -- look up the package in the repo to get all versions that satisfy the version predicate
  constraints <- case get repo pkgRef of

    -- if no package is found under that name, return empty constraints
    Nothing -> do
      -- traceM $ toString pkgRef ++ ": not found."
      return []

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
              Remove ->
                return $ bnot this

              -- if we are adding, check for cyclic dependency (TODO: rethink this)
              Add ->
                if (name, version p) `Set.member` alreadyUsed then do
                  -- traceM $ show $ length alreadyUsed -- "cyclic dependency"
                  return $ bnot this -- add negative constraint because of cyclic dependency

                -- no cyclic dependency, so carry on
                else do

                  -- go over clusters of disjuncts...
                  dependencies <- for (depends p) $ \disjs -> do

                    -- add constraint saying that at least one must be satisfied
                    depDisj <- for disjs (mkConstraints repo memoised (Set.insert (name, version p) alreadyUsed) variables . Do Add)
                    return $ bOr depDisj

                  -- go over all conflicts and require them to be removed (TODO: is this right way?)
                  conflicts <- for (conflicts p) (mkConstraints repo memoised alreadyUsed variables . Do Remove) -- add to already used?

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
    _  -> return $ bOr constraints -- TODO: think about memoisation for efficiency



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


{-
Given initial, target and repo, construct commands

Step 1:
Find State which satisfies target

initial: []
constraints: [ "+A" ]
repo: [ { "name"  : "A"
  , "version" : "2.01"
  , "size" : 1672
  , "depends" :
    [ [ "B>=3.1", "C=1" ]
    , [ "D" ]] }
, { "name" : "B"
  , "version" : "3.0"
  , "size" : 83619 }
, { "name" : "B"
  , "version" : "3.2"
  , "size" : 211234
  , "conflicts" : [ "B<3.2" ] }
, { "name" : "C"
  , "version" : "1"
  , "size" : 23
  , "conflicts" : [ "B" ]
  , "depends" : [] }
, { "name" : "D"
  , "version" : "10.3.1"
  , "size" : 88847
  , "conflicts" : [ "B>=3.1" ] } ]

Ste 2:
construct instructions such that every intermediate state is valid
[ "+C=1"
, "+D=10.3.1"
, "+A=2.01" ]

-}
--
-- initial :: State
-- initial = []
--
-- constraints :: [Constraint]
-- constraints = [(Add, Ref "A" Nothing)]
--
-- repository :: Repo
-- repository = case decode repo of Just r -> r
--   where repo = "[ { \"name\"  : \"A\"\n  , \"version\" : \"2.01\"\n  , \"size\" : 1672\n  , \"depends\" :\n   [ [ \"B>=3.1\", \"C=1\" ]\n    , [ \"D\" ]] }\n, { \"name\" : \"B\"\n  , \"version\" : \"3.0\"\n  , \"size\" : 83619 }\n, { \"name\" : \"B\"\n  , \"version\" : \"3.2\"\n  , \"size\" : 211234\n  , \"conflicts\" : [ \"B<3.2\" ] }\n, { \"name\" : \"C\"\n  , \"version\" : \"1\"\n  , \"size\" : 23\n  , \"conflicts\" : [ \"B\" ]\n  , \"depends\" : [] }\n, { \"name\" : \"D\"\n  , \"version\" : \"10.3.1\"\n  , \"size\" : 88847\n  , \"conflicts\" : [ \"B>=3.1\" ] } ]\n"
--
-- solve :: State -> [Constraint] -> Repo -> [Constraint]
-- solve initial target repo = undefined
