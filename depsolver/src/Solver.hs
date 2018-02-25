{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE Strict #-}
module Solver where

import Data.Monoid
import Data.Traversable (for)
import Data.Ord (comparing)
import qualified Data.Foldable as Foldable (foldl', length)
import Data.List (sortBy, reverse)
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

import Constraints
import Parser

get :: Map Name [Pkg] -> PkgConstr -> [Pkg]
get repo (PkgConstr name versionConstr) =
  case Map.lookup name repo of
    Nothing -> []
    Just pkgs -> throwOut $ pkgs
  where
    throwOut = case versionConstr of
        Nothing -> id
        Just (r,v) -> filter $ \p -> toOperator r (version p) v

mkDepConfConstrs repo pkg = If (Installed pkg) (Conj [deps,confs])
  where
    deps = Conj . map (Disj . map (Disj . (F :) . map (Depends pkg) . get repo)) . depends $ pkg
    confs = Conj . map (Conj . (T :) . map (Conflicts pkg) . get repo) . conflicts $ pkg

mkInitDepConfConstrs :: Map Name [Pkg] -> Pkg -> Constraint Pkg
mkInitDepConfConstrs repo pkg = If (Installed pkg) (Conj [deps,confs])
  where
    deps = Conj . map (Disj . map (Disj . (F :) . map (DependsInit pkg) . get repo)) . depends $ pkg
    confs = Conj . map (Conj . (T :) . map (Conflicts pkg) . get repo) . conflicts $ pkg

process :: Map Name [Pkg] -> [Pkg] -> Set Pkg -> [Constraint Pkg] -> [Constraint Pkg]
process repo toProcess alreadyProcessed constrs =
  case toProcess of
    [] -> constrs
    (p:toProcess) ->
      if Set.member p alreadyProcessed
        then process repo toProcess alreadyProcessed constrs
        else
          let c = mkDepConfConstrs repo p
          in process repo (depsConfls repo p ++ toProcess) (Set.insert p alreadyProcessed) (c:constrs)

depsConfls repo pkg = concatMap (get repo) $ concat (depends pkg) ++ conflicts pkg

subconstrs (Conj xs) = map Conj (subseq xs)
  where
    subseq [] = [[]]
    subseq (x:xs) = map (x :) xss ++ xss
      where xss = subseq xs

go path = do
    (Just hZ3In, Just hZ3Out, _, z3) <- -- Just hZ3Out, Just hZ3Err, z3) <-
      createProcess (proc "z3" ["-smt2","-in"])
      -- createProcess (shell "time z3 -smt2 -in")
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit }

    (repo, initial, target) <- parseInput path

    let initialPkgs = map (\(name,vers) -> head . filter (\p -> version p == vers) $ repo ! name) initial

    initial' <- return . map (mkInitDepConfConstrs repo) $ initialPkgs


    target' <- return . fmap (head . get repo) $ target
    let constrs = process repo (concat (usedIn (fmap (depsConfls repo) target')) ++ usedIn target') (Set.fromList (concatMap usedIn initial')) []
    res <- callSMT True hZ3In hZ3Out (Set.fromList initialPkgs) (target':initial'++constrs)
    case res of
      [] -> error "Failed to satisfy all constraints, retrying on a smaller set of constraints."
      res -> return res


    let initialSet = Set.fromList initial
        pretty acc [] = acc
        pretty acc ((pkg,n):pkgs) =
            if Set.member (fromText pkg) initialSet
              then if n<1 then pretty (("-"<>pkg):acc) pkgs else pretty acc pkgs
              else if n<1 then pretty acc pkgs else pretty (("+"<>pkg):acc) pkgs

    writeFile (path </> "commands.json") (show . reverse . (pretty []) $ res)
    callProcess "python" $ ["tests/judge.py"]
                      ++ map (path </>)
                          [ "repository.json"
                          , "initial.json"
                          , "commands.json"
                          , "constraints.json" ]

    -- putStrLn . unlines $ "Satisfiable:" : out

    -- let targetConstraints = compile repo Installed $ target
    --     pkgs = usedIn targetConstraints
    --
    --     -- TODO: Now, for each package, compile its constraints.
    --
    -- mapM_ (toZ3 . DeclareVar) (concatMap snd . Map.toList $ repo)
    -- toZ3 $ Assert targetConstraints
    -- mapM_ (toZ3 . Assert . compile repo) pkgs
    -- toZ3 Submit
    -- ready <- hWaitForInput hZ3Err (5 * 1000)
    -- when ready $ putStrLn =<< hGetLine hZ3Err
    -- terminateProcess z3
    return ()




callSMT :: Bool -> Handle -> Handle -> Set Pkg -> [Constraint Pkg] -> IO [(Text.Text, Int)]
callSMT logging hIn hOut initial constrs = do

    putStrLn "Calculating constraints and submitting to Z3..."
    t1 <- getCurrentTime
    counter <- newIORef (1 :: Int)

    let toZ3 constraint = do
          let constr = toSmtLib constraint
          when logging $ do
            i <- readIORef counter
            Text.appendFile "log.txt" $ constr <> " ; " <> Text.pack (show i) <> "\n"
            modifyIORef' counter succ
          Text.hPutStrLn hIn constr

        variables :: [Pkg] = usedIn . Conj $ constrs
        costs = map (\p -> if Set.member p initial then (p,0,1000000) else (p,size p,0)) variables

    mapM_ (toZ3 . DeclareVar) $ variables
    -- when (length variables < 2000) (toZ3 $ Objective costs)
    mapM_ (toZ3 . Assert) constrs
    toZ3 Submit
    t2 <- getCurrentTime
    putStr "Constraints submitted to Z3 in "
    putStrLn . show $ diffUTCTime t2 t1


    t1 <- getCurrentTime
    model <- Text.hGetContents hOut
    t2 <- getCurrentTime
    putStrLn $ "Z3 finished in " <> (show $ diffUTCTime t2 t1)
    Text.appendFile "log.txt" model
    -- Text.putStrLn model

    return $ reverse $ (sortBy (comparing (abs . snd)) $ foo [] (Text.lines model))

foo acc [] = acc
foo acc (x:xs) = case Text.stripPrefix "(define-fun " . Text.stripStart $ x of
  Nothing -> foo acc xs
  Just rest ->
    let Right (n,_) = signed decimal . Text.filter (/= '(') . Text.filter (/= ' ') . head $ xs
        pkgId = Text.takeWhile (/= ' ') . Text.filter (/= '|') $ rest
    in foo ((pkgId,n):acc) (tail xs)



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
