{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE Strict #-}
module Solver where

import Control.Monad.IO.Class
import Data.Traversable (for, forM)

import Data.Foldable (foldl')

import qualified Data.Set as Set

import Data.Map.Lazy ((!))
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


-- pass in list of dependencies already checked to stop cycles
mkConstraints :: Repo
  -> Set.Set RefString
  -> IORef (MapL.Map (Name,Version) Predicate)
  -> Set.Set (Name,Version)
  -> IORef (MapL.Map (Name,Version) SBool)
  -> Command
  -> Predicate
mkConstraints repo installed memoised alreadyUsed variables (Do action pkgRef) = do

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
                    return $ bXor depDisj

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
bXor :: Boolean b => [b] -> b
bXor = foldl' (<+>) false

test = optimize Lexicographic $ do
  a <- sBool "A=2.01"
  b <- sBool "B=3.0"
  b' <- sBool "B=3.2"
  c <- sBool "C=1"
  d <- sBool "D=10.3.1"
  assertSoft "B=3.0 is preinstalled" b (Penalty 1000000 Nothing)
  namedConstraint "install A=2.01" a
  namedConstraint "dependencies of A=2.01" (bOr [b', c])
  namedConstraint "dependency of A" d
  namedConstraint "conflict between b and b'" (b' ==> bnot b)
  namedConstraint "conflict between c and b" (c ==> bnot (bOr [b, b']))
  namedConstraint "conflict between d and b'" (d ==> bnot b')

test' = optimize Lexicographic $ do
  a <- sBool "A=2.01"
  b <- sBool "B=3.0"
  b' <- sBool "B=3.2"
  c <- sBool "C=1"
  d <- sBool "D=10.3.1"
  assertSoft "B=3.0 is preinstalled" b (Penalty 1000000 Nothing)
  namedConstraint "install A=2.01" a
  namedConstraint "dependencies of A=2.01" (bOr [b', c])
  namedConstraint "dependency of A" d
  namedConstraint "conflict between b and b'" (b' ==> bnot b)
  namedConstraint "conflict between c and b" (c ==> bnot (bOr [b, b']))
  namedConstraint "conflict between d and b'" (d ==> bnot b')
  let b2n b = ite b 1 0
      cost :: SInteger
      cost = sum $ map b2n [a, b, b', c, d]
  minimize "cost" cost

test'' = optimize Lexicographic $ do
  a <- sBool "A=2.01"
  b <- sBool "B=3.0"
  b' <- sBool "B=3.2"
  c <- sBool "C=1"
  d <- sBool "D=10.3.1"
  assertSoft "B=3.2 is preinstalled" b' (Penalty 1000000 Nothing)
  namedConstraint "install A=2.01" a
  namedConstraint "dependencies of A=2.01" (bOr [b', c])
  namedConstraint "dependency of A" d
  namedConstraint "conflict between b and b'" (b' ==> bnot b)
  namedConstraint "conflict between c and b" (c ==> bnot (bOr [b, b']))
  -- namedConstraint "conflict between d nd b'" (d ==> bnot b')
  let b2n b = ite b 1 0
      cost :: SInteger
      cost = sum $ map b2n [a, b, b', c, d]
  minimize "cost" cost

test''' = optimize Lexicographic $ do
  a <- sBool "A=2.01"
  b <- sBool "B=3.0"
  b' <- sBool "B=3.2"
  c <- sBool "C=1"
  d <- sBool "D=10.3.1"

  namedConstraint "install A=2.01" a
  namedConstraint "dependencies of A=2.01" (bOr [b, b', c])
  namedConstraint "dependency of A" d
  namedConstraint "conflict between b and b'" (b' ==> bnot b)
  namedConstraint "conflict between c and b" (c ==> bnot (bOr [b, b']))
  -- namedConstraint "conflict between d nd b'" (d ==> bnot b')
  let cost :: SInteger
      cost = sum $ map (\(v,c,c') -> ite v c c')
                [(a, 1672, 0), (b, 83619, 0), (b', 0, 1000000), (c, 23, 0), (d, 88847, 0)]
  minimize "cost" cost

test'''' = optimize Lexicographic $ do
  a <- sBool "A=2.01"
  b <- sBool "B=3.0"
  b' <- sBool "B=3.2"
  c <- sBool "C=1"
  d <- sBool "D=10.3.1"

  namedConstraint "install A=2.01" a
  namedConstraint "dependencies of A=2.01" (bOr [b, b', c])
  namedConstraint "dependency of A" d
  namedConstraint "conflict between b and b'" (b' ==> bnot b)
  namedConstraint "conflict between c and b" (c ==> bnot (bOr [b, b']))
  -- namedConstraint "conflict between d nd b'" (d ==> bnot b')
  let cost :: SInteger
      cost = sum $ map (\(v,c,c') -> ite v c c')
                [(a, 1672, 0), (b, 83619, 0), (b', 211234, 0), (c, 23, 0), (d, 88847, 0)]
  minimize "cost" cost

dict :: Symbolic (MapL.Map String SInteger)
dict = do
  vs <- mapM mkVar ["A=2.01", "B=3.2", "B=3.0", "C=1", "D=10.3.1"]
  return $ MapL.fromList vs
  where
    mkVar name = do
        v <- sInteger name
        return (name, v)

test5 = sat $ do
  vs <- dict
  let a = vs ! "A=2.01"
      b = vs ! "B=3.0"
      b' = vs ! "B=3.2"
      c = vs ! "C=1"
      d = vs ! "D=10.3.1"

  namedConstraint "install A=2.01" (a .> 0)
  namedConstraint "dependencies of A=2.01" $ dep a [[b, c],[d]]
  namedConstraint "conflict between c and b" $ confl c [b, b']


dict' :: Symbolic (MapL.Map String (SInteger, (SInteger,SInteger)))
dict' = do
  vs <- mapM mkVar [("A=2.01",(1672,0)),("B=3.2",(83619,0)),("B=3.0",(211234,0)),("C=1",(23,0)),("D=10.3.1",(88847,0))]
  return $ MapL.fromList vs
  where
    mkVar (name,size) = do
        v <- sInteger name
        return (name, (v, size))

test6 = optimize Lexicographic $ do
  vs <- dict'
  let a = fst $ vs ! "A=2.01"
      b = fst $ vs ! "B=3.0"
      b' = fst $ vs ! "B=3.2"
      c = fst $ vs ! "C=1"
      d = fst $ vs ! "D=10.3.1"

  namedConstraint "install A=2.01" (a .> 0)
  namedConstraint "dependencies of A=2.01" $ dep a [[b, c],[d]]
  namedConstraint "conflict between c and b" $ confl c [b, b']

  let cost :: SInteger
      cost = sum . map (\(v,(c,c')) -> ite (v .> 0) c c') . map snd . MapL.toList $ vs
  minimize "cost" cost

dep p dss = bAnd $ map (\ds -> bOr $ map (p .<) ds) dss

confl p cs = (p .> 0) ==> (bAnd $ map (.< 0) cs)



-- test5 = optimize Lexicographic $ do
--   namedConstraint "install A=2.01" a
--   namedConstraint "dependencies of A=2.01" (bOr [b, b', c])
--   namedConstraint "dependency of A" d
--   namedConstraint "conflict between c and b" (c ==> bnot (bOr [b, b']))
--   -- namedConstraint "conflict between d nd b'" (d ==> bnot b')
--   let cost :: SInteger
--       cost = sum $ map (\(v,c,c') -> ite v c c')
--                 [(a, 1672, 0), (b, 83619, 0), (b', 211234, 0), (c, 23, 0), (d, 88847, 0)]
--   minimize "cost" cost
--   where
--     b' = sBool "B=3.2"
--     b = do
--         b <- sBool "B=3.0"
--         b' <- b'
--         namedConstraint "conflict between b and b'" (b' ==> bnot b)
--         return b
