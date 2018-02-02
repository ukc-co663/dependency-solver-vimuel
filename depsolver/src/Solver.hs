-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE Strict #-}
module Solver where

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
    opt (head install)


opt p = optimize Lexicographic $ do

  ps <- installSome p
  let rel = case Plus of Plus -> (.> 0); Minus -> (.< 0)
  constrain $ bOr $ map rel ps
  cost <- liftIO $ readIORef ?cost
  minimize "*cost*" cost

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
            -- unless ((name,version) `Set.member` ?reached) $ do -- avoid going in cycles TODO
            dependencies <- mapM (concatMapM installSome) depends
            conflicts <- concatMapM installSome conflicts
            constrain $ thisOrdering .> 0 ==>
                          bAnd (map (bOr . map (thisOrdering .<)) dependencies)
                          &&& bAnd (map (.< 0) conflicts)



            return $ Just thisOrdering
  where
    concatMapM f =
      foldr (\ x xs -> do x <- f x; if null x then xs else do xs <- xs; return $ x++xs) (return [])

-- -- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ImplicitParams #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- -- {-# LANGUAGE Strict #-}
-- module Solver where
--
-- import Control.Monad.IO.Class
-- import Data.Traversable (for, forM)
--
-- import Data.Foldable (foldl')
--
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.Maybe (catMaybes)
-- import Data.Map.Lazy (Map, (!))
-- import qualified Data.Map.Lazy as Map
-- import Control.Monad (unless)
-- import Data.SBV
-- import Data.IORef
-- import Data.Text (unpack)
-- import Parser
--
-- import Debug.Trace
--
-- import Data.SBV.Control
--
--
-- go path = do
--     repo <- liftIO $ repository path
--     install <- liftIO $ constraints path
--     pkgs <- newIORef Map.empty
--     cost <- newIORef 0
--
--     let ?pkgs = pkgs
--         ?repo = repo
--         ?reached = Set.empty
--         ?cost = cost
--     let p = head install
--     result <- runSMT $ opt p
--     putStrLn result
--
--
--
-- opt p = do
--     setOption $ ProduceUnsatCores True
--     ps <- installSome p
--     let rel = case Plus of Plus -> (.> 0); Minus -> (.< 0)
--     constrain $ bOr $ map rel ps
--     cost <- liftIO $ readIORef ?cost
--     minimize "*cost" cost
--     query $ do
--       cs <- checkSat
--       case cs of
--         Unsat -> unlines <$> getUnsatCore
--         Sat   -> (show . modelAssocs) <$> getModelWithObjectives
--         _ -> error "oops"
--
--           -- optimize Lexicographic $ do
--           --     let ?pkgs = pkgs
--           --         ?repo = repo
--           --         ?reached = Set.empty
--           --         ?cost = cost
--           --     ps <- installSome (fromString "+A")
--           --     let rel = case Plus of Plus -> (.> 0); Minus -> (.< 0)
--           --     constrain $ bOr $ map rel ps
--           --     cost <- liftIO $ readIORef cost
--           --     minimize "*cost*" cost
--
-- get :: (?repo :: Map Name [PkgMeta]) => PkgConstr -> Maybe [PkgMeta]
-- get (PkgConstr name versionConstr) = do
--   pkgs <- Map.lookup name ?repo
--   case versionConstr of
--     Nothing -> return pkgs
--     Just (r, version) -> return $ filter (\PkgMeta{version = v} -> toOperator r v version) pkgs
--
-- installSome :: ( ?repo :: Map Name [PkgMeta]
--                , ?pkgs :: IORef (Map (Name,Version) SInteger)
--                , ?reached :: Set (Name,Version)
--                , ?cost :: IORef SInteger
--                )
--             => (Polarity,PkgConstr) -> Symbolic [SInteger]
-- installSome (polarity,pc@(PkgConstr name _)) =
--   case get pc of
--     Nothing -> return []
--     Just ps -> catMaybes <$> mapM (install name) ps
--       -- ps <- catMaybes <$> mapM (install name) ps
--       -- let rel = case polarity of Plus -> (.> 0); Minus -> (.< 0)
--       -- constrain $ bOr $ map rel ps
--       -- return ps
--
--
--         -- constrain $ ordering .> 0
--
-- install :: ( ?repo :: Map Name [PkgMeta]
--            , ?pkgs :: IORef (Map (Name,Version) SInteger)
--            , ?reached :: Set (Name,Version)
--            , ?cost :: IORef SInteger
--            )
--         => Name -> PkgMeta -> Symbolic (Maybe SInteger)
-- install name meta@(PkgMeta refString version size depends conflicts installed) =
--     if not installed && (name,version) `Set.member` ?reached
--       then return Nothing -- cyclical dependency
--       else let ?reached = Set.insert (name,version) ?reached in do
--         pkgs <- liftIO $ readIORef ?pkgs
--         case Map.lookup (name, version) pkgs of
--           Just p -> return $ Just p
--           Nothing -> do
--             thisOrdering <- sInteger $ unpack refString
--             liftIO $ modifyIORef ?pkgs $ Map.insert (name,version) thisOrdering
--
--             let costT = if installed then       0 else fromInteger size
--                 costF = if installed then 1000000 else 0
--                 cost = ite (thisOrdering .> 0) costT costF
--             liftIO $ modifyIORef ?cost (+ cost)
--             -- unless ((name,version) `Set.member` ?reached) $ do -- avoid going in cycles TODO
--             dependencies <- mapM (concatMapM installSome) depends
--             conflicts <- concatMapM installSome conflicts
--             namedConstraint ("Dependencies for " ++ unpack refString)
--                             $ thisOrdering .> 0 ==>
--                                 bAnd (map (bOr . map (thisOrdering .<)) dependencies)
--             namedConstraint ("Conflicts for " ++ unpack refString)
--                             $ thisOrdering .> 0 ==> bAnd (map (.< 0) conflicts)
--
--
--
--             return $ Just thisOrdering
--   where
--     concatMapM f =
--       foldr (\ x xs -> do x <- f x; if null x then xs else do xs <- xs; return $ x++xs) (return [])
--
--
--         -- let costT = if installed then       0 else fromInteger size
--         --     costF = if installed then 1000000 else 0
--         --     cost = ite (ordering .> 0) costT costF
--
--             -- liftIO $ modifyIORef TODO
--
--
--
--
--
-- -- go :: IO OptimizeResult
-- -- go = optimize Lexicographic $ do
-- --     vs <- dict'
-- --     mkConstr vs
-- --     let cost :: SInteger
-- --         cost = sum . map (\(v,(c,c')) -> ite (v .> 0) c c') . map snd . MapL.toList $ vs
-- --     minimize "*cost*" cost
--
--
--
--
--
--
-- {- static example
--
-- dict' :: Symbolic (MapL.Map String (SInteger, (SInteger,SInteger)))
-- dict' = do
--   vs <- mapM mkVar [("A=2.01",(1672,0)),("B=3.2",(83619,0)),("B=3.0",(211234,0)),("C=1",(23,0)),("D=10.3.1",(88847,0))]
--   return $ MapL.fromList vs
--   where
--     mkVar (name,size) = do
--         v <- sInteger name
--         return (name, (v, size))
--
-- go :: IO OptimizeResult
-- go = optimize Lexicographic $ do
--     vs <- dict'
--     mkConstr vs
--     let cost :: SInteger
--         cost = sum . map (\(v,(c,c')) -> ite (v .> 0) c c') . map snd . MapL.toList $ vs
--     minimize "*cost*" cost
--
-- mkConstr vs = do
--   let a = fst $ vs ! "A=2.01"
--       b = fst $ vs ! "B=3.0"
--       b' = fst $ vs ! "B=3.2"
--       c = fst $ vs ! "C=1"
--       d = fst $ vs ! "D=10.3.1"
--
--   namedConstraint "install A=2.01" (a .> 0)
--   namedConstraint "dependencies of A=2.01" $ dep a [[b, c],[d]]
--   namedConstraint "conflict between c and b" $ confl c [b, b']
--
-- dep p dss = bAnd $ map (\ds -> bOr $ map (p .<) ds) dss
--
-- confl p cs = (p .> 0) ==> (bAnd $ map (.< 0) cs)
--
-- -}
