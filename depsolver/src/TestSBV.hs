module TestSBV where

import Data.SBV
import Data.SBV.Control
-- 
-- test = do
--   setOption $ ProduceUnsatCores True
--
--   mbCore <- runSMT p
--   case mbCore of
--     Nothing   -> putStrLn "Problem is satisfiable."
--     Just core -> putStrLn $ "Unsat core is: " ++ show core
--
-- p = do
--   setOption $ ProduceUnsatCores True
--   a <- sInteger "a"
--   namedConstraint "less than 5" $ a .< 5
--   query $ do
--     cs <- checkSat
--     case cs of
--       Sat -> Just <$> getModel
--       Unsat -> Just <$> getUnsatCore
--       Unk     -> return Nothing
