
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE Strict #-}
module SolverUnsat where

import Control.Monad.IO.Class
import Data.Traversable (for, forM)

import Data.Foldable (foldl')

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Control.Monad (unless)
import Data.SBV
import Data.IORef
import Data.Text (unpack)
import Parser

import Debug.Trace

import Data.SBV.Control


go path = do
    repo <- liftIO $ repository path
    install <- liftIO $ constraints path
    pkgs <- newIORef Map.empty
    cost <- newIORef 0

    let ?pkgs = pkgs
        ?repo = repo
        ?reached = Set.empty
        ?cost = cost
    let p = head install
    result <- runSMT $ opt p
    putStrLn result



opt p = do
    setOption $ ProduceUnsatCores True
    ps <- installSome p
    let rel = case Plus of Plus -> (.> 0); Minus -> (.< 0)
    constrain $ bOr $ map rel ps
    cost <- liftIO $ readIORef ?cost
    minimize "*cost" cost
    query $ do
      cs <- checkSat
      case cs of
        Unsat -> unlines <$> getUnsatCore
        Sat   -> show <$> getModel
        _ -> error "oops"

          -- optimize Lexicographic $ do
          --     let ?pkgs = pkgs
          --         ?repo = repo
          --         ?reached = Set.empty
          --         ?cost = cost
          --     ps <- installSome (fromString "+A")
          --     let rel = case Plus of Plus -> (.> 0); Minus -> (.< 0)
          --     constrain $ bOr $ map rel ps
          --     cost <- liftIO $ readIORef cost
          --     minimize "*cost*" cost

get :: (?repo :: Map Name [PkgMeta]) => PkgConstr -> Maybe [PkgMeta]
get (PkgConstr name versionConstr) = do
  pkgs <- Map.lookup name ?repo
  case versionConstr of
    Nothing -> return pkgs
    Just (r, version) -> return $ filter (\PkgMeta{version = v} -> toOperator r v version) pkgs

installSome :: ( ?repo :: Map Name [PkgMeta]
               , ?pkgs :: IORef (Map (Name,Version) SInteger)
               , ?reached :: Set (Name,Version)
               , ?cost :: IORef SInteger
               )
            => (Polarity,PkgConstr) -> Symbolic [SInteger]
installSome (polarity,pc@(PkgConstr name _)) =
  case get pc of
    Nothing -> return []
    Just ps -> catMaybes <$> mapM (install name) ps
      -- ps <- catMaybes <$> mapM (install name) ps
      -- let rel = case polarity of Plus -> (.> 0); Minus -> (.< 0)
      -- constrain $ bOr $ map rel ps
      -- return ps


        -- constrain $ ordering .> 0

install :: ( ?repo :: Map Name [PkgMeta]
           , ?pkgs :: IORef (Map (Name,Version) SInteger)
           , ?reached :: Set (Name,Version)
           , ?cost :: IORef SInteger
           )
        => Name -> PkgMeta -> Symbolic (Maybe SInteger)
install name meta@(PkgMeta refString version size depends conflicts installed) =
    if not installed && (name,version) `Set.member` ?reached
      then return Nothing -- cyclical dependency
      else let ?reached = Set.insert (name,version) ?reached in do
        pkgs <- liftIO $ readIORef ?pkgs
        case Map.lookup (name, version) pkgs of
          Just p -> return $ Just p
          Nothing -> do
            thisOrdering <- sInteger $ unpack refString
            liftIO $ modifyIORef ?pkgs $ Map.insert (name,version) thisOrdering

            let costT = if installed then       0 else fromInteger size
                costF = if installed then 1000000 else 0
                cost = ite (thisOrdering .> 0) costT costF
            liftIO $ modifyIORef ?cost (+ cost)
            unless ((name,version) `Set.member` ?reached) $ do -- avoid going in cycles TODO
              dependencies <- mapM (concatMapM installSome) depends
              conflicts <- concatMapM installSome conflicts
              namedConstraint ("Dependencies for " ++ unpack refString)
                              $ thisOrdering .> 0 ==>
                                  bAnd (map (bOr . map (thisOrdering .<)) dependencies)
              namedConstraint ("Conflicts for " ++ unpack refString)
                              $ thisOrdering .> 0 ==> bAnd (map (.== 0) conflicts)



            return $ Just thisOrdering
  where
    concatMapM f =
      foldr (\ x xs -> do x <- f x; if null x then xs else do xs <- xs; return $ x++xs) (return [])
