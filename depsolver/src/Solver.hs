{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE Strict #-}
module Solver where

import Data.Monoid
import Control.Monad.IO.Class
import Data.Traversable (for, forM)
import Data.Ord (comparing)
import qualified Data.Foldable as Foldable (foldl', length)
import Data.List (sortBy, reverse)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, mapMaybe, isNothing, fromJust)
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Control.Monad (unless, forM, forM_, when)
import Data.SBV
import qualified Data.SBV.Internals as SBV
import Data.IORef
import System.FilePath ((</>))
import System.Process
import Data.List (sortBy)
import Data.Ord (comparing)
import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Debug.Trace

import Constraints
import Parser

-- Conj [Installed "A", Not (Installed "B")]

-- compilePkgConstr :: Map Name [Pkg] -> Constraint PkgConstr -> Constraint Pkg
-- compilePkgConstr repo constr =
--   case constr of
--     Installed (PkgConstr name versionConstr) ->
--       case Map.lookup name repo of
--         Nothing -> F
--         Just
--     Conj cs -> Conj . map (compilePkgConstr repo) $ cs
--     Disj cs -> Disj . map (compilePkgConstr repo) $ cs
--     Not c -> Not . compilePkgConstr repo $ c
--     T -> T
--     _ -> F
{--
```Depends (Any "A") (Any "B")```
where `Any "A"` is some key, that maps to, say, `Disj ["A1", "A2"]`. So I `fmap`my lookup function and get:
```Depends (Disj ["A1", "A2"]) (Disj ["B1","B2"])
```
Then, I want my `join` to give me the disjunction of the cartesian product of the dependencies
```Disj [Depends "A1" "B1",Depends "A1" "B2",Depends "A2" "B1",Depends "A2" "B2"]```

Depends (Conj ["
--}

class Compile a where
  compile :: Map Name [Pkg] -> a -> Constraint Pkg

instance Compile (Constraint PkgConstr) where
  compile repo = join . fmap (compile repo)

instance Compile PkgConstr where
  compile repo (PkgConstr name versionConstr) =
    case Map.lookup name repo of
      Nothing -> F
      Just pkgs -> Disj . map Installed . throwOut $ pkgs
    where
      throwOut = case versionConstr of
          Nothing -> id
          Just (r,v) -> filter $ \p -> toOperator r v (version p)

instance Compile Pkg where
  compile repo pkg = If (Installed pkg) (Conj [deps pkg,confs pkg])
    where
      deps = Conj . map (Disj . map compileDs) . depends
      confs = Conj . map compileCs . conflicts

      compileDs (PkgConstr name versionConstr) =
        case Map.lookup name repo of
          Nothing -> F
          Just pkgs -> Disj . map (Depends pkg) . throwOut $ pkgs
        where
          throwOut = case versionConstr of
              Nothing -> id
              Just (r,v) -> filter $ \p -> toOperator r v (version p)

      compileCs (PkgConstr name versionConstr) =
        case Map.lookup name repo of
          Nothing -> F
          Just pkgs -> Disj . map (Conflicts pkg) . throwOut $ pkgs
        where
          throwOut = case versionConstr of
              Nothing -> id
              Just (r,v) -> filter $ \p -> toOperator r v (version p)


instance ToSmtLib SmtLibStmt where
  toSmtLib (Assert c) = "(assert " <> toSmtLib (simplifyDeep c) <> ")"
  toSmtLib (DeclareVar v) = "(declare-const " <> toSmtLib v <> " Int)"
  toSmtLib Submit = "(check-sat)\n(get-model)\n(exit)"

data SmtLibStmt
  = Assert (Constraint Pkg)
  | DeclareVar Pkg
  | Submit

go path = do
    (Just hZ3In, _, _, z3) <- -- Just hZ3Out, Just hZ3Err, z3) <-
      -- createProcess (proc "z3" ["-smt2","-in"])
      createProcess (shell "time z3 -smt2 -in")
        { std_in  = CreatePipe
        , std_out = Inherit
        , std_err = Inherit }

    counter <- newIORef (1 :: Int)
    let logging = True
    let toZ3 constraint = do
          let constr = toSmtLib constraint
          when logging $ do
            i <- readIORef counter
            Text.appendFile "log.txt" $ constr <> " ; " <> Text.pack (show i) <> "\n"
            modifyIORef' counter succ
          Text.hPutStrLn hZ3In constr


    (repo, initial, target) <- parseInput path


    let targetConstraints = compile repo $ target
        pkgs = usedIn targetConstraints

        -- TODO: Now, for each package, compile its constraints.

    mapM_ (toZ3 . DeclareVar) (concatMap snd . Map.toList $ repo)
    toZ3 $ Assert targetConstraints
    mapM_ (toZ3 . Assert . compile repo) pkgs
    toZ3 Submit



    -- ready <- hWaitForInput hZ3Err (5 * 1000)
    -- when ready $ putStrLn =<< hGetLine hZ3Err


    hClose hZ3In
    putStrLn "Finished."
    -- terminateProcess z3
    return ()


-- getConstraints :: (?store :: IORef (Map (name,version) Text)) Name -> Version -> Text
-- getConstraints (name,version) = do
--   store <- readIORef ?store
--   case Map.lookup (name,version) store of
--     Just p ->
--       return p
--     Nothing ->
--
--       modifyIORef ?store (Map.insert )

--
-- dependsConstraints p = mkConstr deps
--   where
--     deps = map (map get) (depends p) -- TODO (filter installed ?
--     mkConstr =
--       assert . ifInstalled this . conj . map (disj . map (disj . map ((this `dependsOn`) . refString)))
--     this = refString p
--
-- conflConstraints p = mkConstr confls
--   where
--     confls = map get (conflicts p)
--     mkConstr =
--       assert . ifInstalled this . conj . map (conj . map ((this `conflictsWith`) . refString))
--     this = refString p
--
-- -- assertInstalled ps =
-- --   where
-- --
--
-- ifInstalled p q = case q of
--   "" -> ""
--   q -> "(=> (< 0 " <> p <> ") " <> q <> ")"
--
-- dependsOn x "" = ""
-- dependsOn x y = "(< " <> x <> " " <> y <> ")"
--
-- conflictsWith x "" = ""
-- conflictsWith x y = "(< " <> y <> " (- " <> x <> "))"
--
-- disj ps = case filter (/= "") ps of
--   [] -> ""
--   [p] -> p
--   ps -> "(or " <> Text.intercalate " " ps <> ")"
--
-- conj ps = case filter (/= "") ps of
--   [] -> ""
--   [p] -> p
--   ps -> "(and " <> Text.intercalate " " ps <> ")"
--
-- assert x = case x of
--   "" -> ""
--   x -> "(assert " <> x <> ")"
--
-- assertInstalled x = "(assert (< 0 " <> x <> "))"
--
-- -- not x = "(not " <> x <> ")"
--
-- declarePkg x = "(declare-const " <> x <> " Int)"
--
--



    {-
go path = do
    repo <- liftIO $ repository path
    initial <- liftIO $ initial path
    constraints <- liftIO $ constraints path
    pkgs <- newIORef Map.empty
    cost <- newIORef 0

    let ?pkgOrds = pkgs
        ?repo = repo
        ?reached = Map.empty
        ?initial = Set.toList initial
        ?cost = cost
    r@(LexicographicResult result) <- opt constraints
    print r
    case result of
      Unsatisfiable _ -> putStrLn "Unsatisfiable"
      Satisfiable _ model@(SBV.SMTModel obj ass) -> do
        let result = sortBy (flip (comparing (abs . snd))) $ map (\(a,b) -> (a,fromCW b :: Integer)) ass
        let out = mapMaybe (\(pkgRef, ord) ->
                    case get (fromString pkgRef) of
                      Just [pkg]
                        | ord <= 0 && installed pkg -> Just $ '-':pkgRef
                        | ord > 0  && not (installed pkg) -> Just $ '+':pkgRef
                        | otherwise -> Nothing
                      Nothing -> case pkgRef of
                        "*cost*" -> Nothing
                        _ -> error $ "Unknown package: " ++ pkgRef
                      _ -> error "Unreachable"
                    ) result
        writeFile (path </> "commands.json") (show out)
        callProcess "python" $ ["tests/judge.py"]
                          ++ map (path </>)
                              [ "repository.json"
                              , "initial.json"
                              , "commands.json"
                              , "constraints.json" ]
        -- putStrLn . unlines $ "Satisfiable:" : out


opt ps = optimize Lexicographic $ do
  traceM $ show $ length ?initial
  forM_ (zip ?initial [1..]) $ \(p,n) -> do
    -- traceM $ unpack p ++ " is in initial."
    [p] <- installInstall $ fromText p
    -- when (n `mod` 100 == 0) (traceM $ show n
    traceM $ show n
    return ()

  forM_ ps $ \p@(polarity,constr) -> do
    ps <- map fst <$> installInstall constr
    case polarity of
      Plus -> constrain $ bOr $ map (.> 0) ps
      Minus -> constrain $ bAnd $ map (.< 0) ps

  cost <- liftIO $ readIORef ?cost
  minimize "*cost*" cost


-}

{- ------------------------
get :: (?repo :: Map Name [Pkg]) => PkgConstr -> [Pkg]
get (PkgConstr name versionConstr) =
  case Map.lookup name ?repo of
    Nothing -> []
    Just pkgs ->
      case versionConstr of
        Nothing -> pkgs
        Just (r, version) -> filter (\Pkg{version = v} -> toOperator r v version) pkgs

-- installInstall :: ( ?repo :: Map Name [Pkg]
--                , ?pkgOrds :: IORef (Map (Name,Version) SInteger)
--                , ?reached :: Set ((Name,Version), Maybe (Name,Version))
--                , ?cost :: IORef SInteger )
--             => PkgConstr -> Symbolic [SInteger]
mkConstraints pc@(PkgConstr name _) = mapM (mkConstraint name) (get pc)

getSymbolicOrdering name (Pkg refString version size _ _ installed) = do
    pkgOrds <- liftIO $ readIORef ?pkgOrds
    case Map.lookup (name, version) pkgOrds of
      Just p -> return p
      Nothing -> do
        p <- sInteger $ unpack refString
        liftIO $ modifyIORef ?pkgOrds $ Map.insert (name,version) p
        let costT = if installed then       0 else fromInteger size
            costF = if installed then 1000000 else 0
            cost = ite (p .> 0) costT costF
        liftIO $ modifyIORef ?cost (+ cost)
        return p

mkConstraint :: ( ?repo :: Map Name [Pkg]
           , ?pkgOrds :: IORef (Map (Name,Version) SInteger)
           , ?reached :: Map (Name,Version) (Bool, SInteger)
           , ?cost :: IORef SInteger )
        => Name -> Pkg -> Symbolic ()
mkConstraint name meta@(Pkg refString version size depends conflicts inInitial) = do
    thisOrdering <- getSymbolicOrdering name meta

    dependencies <- mapM (concatMapM getSymbolicOrdering) depends
    conflicts <- map fst <$> concatMapM installInstall conflicts
    -- traceM $ show conflicts ++ " conflict with " ++ show (name, version)
    constrain $ thisOrdering .> 0 ==>
                  bAnd (map (bOr . map (\(dep, dInInit)
                    -> if dInInit then thisOrdering .<= dep else thisOrdering .< dep)) dependencies)
                  &&& bAnd (map (0 - thisOrdering .>) conflicts)
-----------------------------------------------}

{-
install :: ( ?repo :: Map Name [Pkg]
           , ?pkgOrds :: IORef (Map (Name,Version) SInteger)
           , ?reached :: Map (Name,Version) (Bool, SInteger)
           , ?cost :: IORef SInteger )
        => Name -> Pkg -> Symbolic (SInteger, Bool)
install name meta@(Pkg refString version size depends conflicts inInitial) = do

    thisOrdering <- getSymbolicOrdering name meta

    case Map.lookup (name,version) ?reached of
      Just (otherInInitial, otherOrdering) -> do
        -- traceM $ "Cyclical dependency: " ++ show (name,version)
        -- if inInitial && otherInInitial
        --   then constrain $ thisOrdering .== otherOrdering
        --   else constrain $ thisOrdering .< 0 -- cyclical dependency
        constrain $ thisOrdering .== otherOrdering &&& thisOrdering .> 0


      Nothing -> let ?reached = Map.insert (name,version) (inInitial, thisOrdering) ?reached in do


        -- unless (installed && (name,version) `Set.member` ?reached) $ do -- avoid going in cycles TODO

        dependencies <- mapM (concatMapM installInstall) depends
        conflicts <- map fst <$> concatMapM installInstall conflicts
        -- traceM $ show conflicts ++ " conflict with " ++ show (name, version)
        constrain $ thisOrdering .> 0 ==>
                      bAnd (map (bOr . map (\(dep, dInInit)
                        -> if dInInit then thisOrdering .<= dep else thisOrdering .< dep)) dependencies)
                      &&& bAnd (map (0 - thisOrdering .>) conflicts)

    return (thisOrdering, inInitial)

  where
    concatMapM f =
      foldr (\ x xs -> do x <- f x; if null x then xs else do xs <- xs; return $ x++xs) (return [])
-}



{-
install :: ( ?repo :: Map Name [Pkg]
           , ?pkgOrds :: IORef (Map (Name,Version) SInteger)
           , ?reached :: Set (Name,Version)
           , ?cost :: IORef SInteger
           )
        => Name -> Pkg -> Symbolic (Maybe SInteger)
install name meta@(Pkg refString version size depends conflicts installed) =
    if {- not installed && -} (name,version) `Set.member` ?reached
      then do
        liftIO $ putStrLn $ "Cyclical dependency: " ++ unpack name ++ toString version
        return $ Just 0 -- cyclical dependency
      else let ?reached = Set.insert (name,version) ?reached in do
        pkgs <- liftIO $ readIORef ?pkgOrds
        case Map.lookup (name, version) pkgs of
          Just p -> return $ Just p
          Nothing -> do
            thisOrdering <- sInteger $ unpack refString
            liftIO $ modifyIORef ?pkgOrds $ Map.insert (name,version) thisOrdering

            let costT = if installed then       0 else fromInteger size
                costF = if installed then 1000000 else 0
                cost = ite (thisOrdering .> 0) costT costF
            liftIO $ modifyIORef ?cost (+ cost)
            -- unless (installed && (name,version) `Set.member` ?reached) $ do -- avoid going in cycles TODO
            dependencies <- mapM (concatMapM installInstall) depends
            conflicts <- concatMapM installInstall conflicts
            traceM $ show conflicts ++ " conflict with " ++ show (name, version)
            constrain $ thisOrdering .> 0 ==>
                          bAnd (map (bOr . map (thisOrdering .<)) dependencies)
                          &&& bAnd (map (0 - thisOrdering .>) conflicts)



            return $ Just thisOrdering
  where
    concatMapM f =
      foldr (\ x xs -> do x <- f x; if null x then xs else do xs <- xs; return $ x++xs) (return [])
-}
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
