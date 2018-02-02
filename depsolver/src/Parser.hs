{-# language OverloadedStrings #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}

module Parser
  ( decode
  , module Parser
  , T.unpack
  )
 where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.Char (isAlphaNum, isDigit)
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.FilePath ((</>))
import Data.SBV

import qualified Data.ByteString.Lazy as B
import Data.Foldable (foldl')

import Debug.Trace

class ToText a where
  toText :: a -> Text
  toString :: a -> String
  toString = T.unpack . toText

data Polarity = Plus | Minus deriving Show

instance ToText (Polarity, PkgConstr) where
  toText (Plus, r) = "+" <> toText r
  toText (Minus, r) = "-" <> toText r

type Name = Text

type RefString = Text

data PkgConstr = PkgConstr Name (Maybe (Relation, Version)) deriving Show

instance ToText PkgConstr where
  toText (PkgConstr name (Just (rel, vers))) = name <> toText rel <> toText vers
  toText (PkgConstr name _) = name

data Relation =  Lt | LtEq | Eq | GtEq | Gt deriving Show

instance ToText Relation where
  toText r = case r of
    Lt -> "<"
    LtEq -> "<="
    Eq -> "="
    GtEq -> ">="
    Gt -> ">"

-- toOperator :: Relation ->
toOperator r = case r of
  Lt -> (<)
  LtEq -> (<=)
  Eq -> (==)
  GtEq -> (>=)
  Gt -> (>)

type Version = [Word]

instance ToText Version where
  toText = T.intercalate "." . map (T.pack . show)

-- data Pkg = Pkg
--   { name :: Name
--   , meta :: PkgMeta
--   , ordering :: SInteger
--   , cost :: SInteger
--   } deriving Show

data PkgMeta = PkgMeta
  { refString :: Text
  , version :: Version
  , size :: Integer
  , depends :: [[(Polarity,PkgConstr)]]
  , conflicts :: [(Polarity,PkgConstr)]
  , installed :: Bool
  } deriving Show

repository :: FilePath -> IO (Map Name [PkgMeta])
repository wd = do
  raw <- B.readFile $ wd </> "repository.json"
  initial <- initial wd
  return $ parseRepo raw initial

constraints :: FilePath -> IO [(Polarity,PkgConstr)]
constraints wd = do
  raw <- B.readFile $ wd </> "constraints.json"
  let Just cs = decode raw :: Maybe [Text]
  return $ map fromText cs

initial :: FilePath -> IO (Set Text)
initial wd = do
  raw <- B.readFile $ wd </> "initial.json"
  let Just cs = decode raw :: Maybe [Text]
  -- return $ Set.fromList $ map toRef cs
  return $ Set.fromList cs

instance {-# overlaps #-} FromJSON (Name, PkgMeta) where
  parseJSON = withObject "Package" $ \obj -> do
    name <- obj .: "name"
    version <- fmap fromText $ obj .: "version"
    let refString = toText $ PkgConstr name (Just (Eq, version))
    size <- obj .: "size"
    dep <- obj .:? "depends" .!= []
    -- let depends = map (map (\constr -> (Plus,fromText constr))) dep :: [[(Polarity,PkgConstr)]]
    let depends = map (map ((,) Plus . fromText)) dep :: [[(Polarity,PkgConstr)]]
    con <- obj .:? "conflicts" .!= []
    let conflicts = map ((,) Minus . fromText) con
    return (name, PkgMeta refString version size depends conflicts False)

parseRepo :: B.ByteString -> Set Text -> Map Name [PkgMeta]
parseRepo raw initial =
    let inp = V.toList $ fromJust $ decode raw :: [(Name, PkgMeta)] in
    let pkgs = map (\(n,m) -> (n,[m{installed = refString m `Set.member` initial}])) inp in
    fmap (sortBy $ comparing version) $ Map.fromListWith (++) pkgs


-- parseRepo :: B.ByteString -> Set Text -> Symbolic (Map Name [Pkg])
-- parseRepo raw initial = do
--     let inp = V.toList $ fromJust $ decode raw :: [(Name, PkgMeta)]
--     pkgs <- mapM toPkg inp
--     return $ fmap (sortBy $ comparing (version . meta)) $ Map.fromListWith (++) pkgs
--   where
--     toPkg :: (Name, PkgMeta) -> Symbolic (Name, [Pkg])
--     toPkg (name, meta) = do
--         ordering <- sInteger (T.unpack $ refString meta)
--         let installed = refString meta `Set.member` initial
--             costT = if installed then       0 else fromInteger $ size meta
--             costF = if installed then 1000000 else 0
--             cost = ite (ordering .> 0) costT costF
--         return $ (name, [Pkg name meta ordering installed cost])

class FromText a where
  fromText :: Text -> a
  fromString :: String -> a
  fromString a = fromText (T.pack a)

instance FromText Version where
  fromText "" = []
  fromText str = map unwrap $ T.splitOn "." str where unwrap x = case decimal x of Right (n,_) -> n

instance FromText PkgConstr where
  fromText inp = case T.span (\c -> isAlphaNum c || (c == '-') || (c == '.') || (c == '+')) inp of
    (name,"") -> PkgConstr name Nothing
    (name,rest) -> case T.break (\c -> isDigit c || (c == '.')) rest of
      (rel,vers) -> PkgConstr name (Just (fromText rel, fromText vers))

instance FromText Relation where
  fromText inp = case inp of
    "<" -> Lt
    "<=" -> LtEq
    "=" -> Eq
    ">=" -> GtEq
    ">" -> Gt

instance FromText (Polarity,PkgConstr) where
  fromText inp = case T.uncons inp of
    Just ('+', rest) -> (Plus,(fromText rest))
    Just ('-', rest) -> (Minus,(fromText rest))
