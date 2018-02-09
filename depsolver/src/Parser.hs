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

import Constraints

class ToText a where
  toText :: a -> Text
  toString :: a -> String
  toString = T.unpack . toText

type Name = Text

type PkgId = (Name, Version)

instance ToSmtLib PkgId where
  toSmtLib pkgId = "|" <> toText pkgId <> "|"

instance ToSmtLib Pkg where
  toSmtLib = toSmtLib . pkgId

instance ToText PkgId where
  toText (name,version) = name <> "=" <> toText version

data PkgConstr = PkgConstr Name (Maybe (Relation, Version)) deriving Show

-- instance ToText PkgConstr where
--   toText (PkgConstr name (Just (rel, vers))) = name <> toText rel <> toText vers
--   toText (PkgConstr name _) = name

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

data Pkg = Pkg
  { pkgId :: (Name, Version)
  , size :: Integer
  , depends :: [[PkgConstr]]  -- [[PkgConstr]]
  , conflicts :: [PkgConstr]  -- [PkgConstr]
  } deriving Show

instance Eq Pkg where
  p == q = pkgId p == pkgId q

instance Ord Pkg where
  compare p q = compare (pkgId p) (pkgId q)

name :: Pkg -> Name
name = fst . pkgId

version :: Pkg -> Version
version = snd . pkgId

parseInput :: FilePath -> IO (Map Name [Pkg],Set PkgId,Constraint PkgConstr)
parseInput wd = do
  r <- repository wd
  i <- initial wd
  c <- constraints wd
  return (r,i,c)

repository :: FilePath -> IO (Map Name [Pkg])
repository wd = do
  raw <- B.readFile $ wd </> "repository.json"
  initial <- initial wd
  let parseRepo = V.toList . fromJust . decode
      mkRepo = Map.fromListWith (++) . map (\(n,m) -> (n,[m]))
  return . mkRepo . parseRepo $ raw

constraints :: FilePath -> IO (Constraint PkgConstr)
constraints wd = do
  raw <- B.readFile $ wd </> "constraints.json"
  let Just cs = decode raw :: Maybe [Text]
  return . Conj $ map fromText cs

initial :: FilePath -> IO (Set PkgId)
initial wd = do
  raw <- B.readFile $ wd </> "initial.json"
  let Just cs = decode raw
  return . Set.fromList . map fromText $ cs
  -- return $ Set.fromList cs
  -- return cs

instance {-# overlaps #-} FromJSON (Name, Pkg) where
  parseJSON = withObject "Package" $ \obj -> do
    name <- obj .: "name"
    version <- fmap fromText $ obj .: "version"
    size <- obj .: "size"
    depends <- map (map (fromText)) <$> obj .:? "depends" .!= [] ---- TODO Empty should be T!!!!!!!!!!!!!!!!!!!!
    conflicts <- map (fromText) <$> obj .:? "conflicts" .!= [] ---- TODO Empty should be T!!!!!!!!!!!!!!!!!!!!
    return (name, Pkg (name,version) size depends conflicts)


class FromText a where
  fromText :: Text -> a
  fromString :: String -> a
  fromString a = fromText (T.pack a)

instance FromText Version where
  fromText "" = []
  fromText str = map unwrap $ T.splitOn "." str where unwrap x = case decimal x of Right (n,_) -> n

instance FromText PkgId where
  fromText inp = case T.span (\c -> isAlphaNum c || (c == '-') || (c == '.') || (c == '+')) inp of
    (name,rest) -> case T.uncons rest of
      Just ('=',vers) -> (name,fromText vers)
      Nothing -> error "unexpected package id format."

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

instance FromText (Constraint PkgConstr) where
  fromText inp = case T.uncons inp of
    Just ('+', rest) -> Installed (fromText rest)
    Just ('-', rest) -> Not $ Installed (fromText rest)
    _ -> error $ show inp



-- mkConditionalConstraint :: a -> Constraint a -> Constraint a -> Constraint a
-- mkConditionalConstraint p c = If (Installed p) (Conj [deps,confls])
