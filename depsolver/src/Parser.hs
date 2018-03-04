{-# language OverloadedStrings #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.List (sortBy, partition)
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
import Data.List (intersperse)

import qualified Data.ByteString.Lazy as B
import Data.Foldable (foldl')

class ToText a where
  toText :: a -> Text
  toString :: a -> String
  toString = T.unpack . toText

type Name = Text

type PkgId = (Name, Version)

instance ToText Pkg where
  toText = toText . pkgId

instance ToText PkgId where
  toText (name,version) = name <> "=" <> toText version

data PkgConstr = PkgConstr Name (Maybe (Relation, Version)) deriving Show

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
  , confls :: [PkgConstr]  -- [PkgConstr]
  } deriving Show

instance Eq Pkg where
  p == q = pkgId p == pkgId q

instance Ord Pkg where
  compare p q = compare (pkgId p) (pkgId q)

name :: Pkg -> Name
name = fst . pkgId

version :: Pkg -> Version
version = snd . pkgId

parseInput :: FilePath -> IO ([(Name,Pkg)],Set PkgId,Pkg)
parseInput wd = do
  r <- repository wd
  i <- initial wd
  c <- constraints wd
  return (r,Set.fromList i,c)

repository :: FilePath -> IO [(Name,Pkg)]
repository wd = do
  raw <- B.readFile $ wd </> "repository.json"
  -- let parseRepo = V.toList . fromJust . decode
  --     mkRepo = Map.fromListWith (++) . map (\(n,m) -> (n,[m]))
  -- return . mkRepo . parseRepo $ raw
  let parseRepo = V.toList . fromJust . decode
  return . parseRepo $ raw

constraints :: FilePath -> IO Pkg
constraints wd = do
  raw <- B.readFile $ wd </> "constraints.json"
  let Just target = decode raw :: Maybe [Text]
      (ds,cs) = partition (\c -> case T.uncons c of Just ('+', _) -> True; Just ('-', _) -> False) target
      deps = map (return . fromText . T.tail) ds
      conf = map (fromText . T.tail) cs
  return Pkg { pkgId = ("_VIRTUAL_",[]), size = 0, depends = deps, confls = conf }

initial :: FilePath -> IO ([PkgId])
initial wd = do
  raw <- B.readFile $ wd </> "initial.json"
  let Just cs = decode raw
  return . map fromText $ cs

instance {-# overlaps #-} FromJSON (Name, Pkg) where
  parseJSON = withObject "Package" $ \obj -> do
    name <- obj .: "name"
    version <- fmap fromText $ obj .: "version"
    size <- obj .: "size"
    depends <- map (map (fromText)) <$> obj .:? "depends" .!= []
    confls <- map (fromText) <$> obj .:? "conflicts" .!= []
    return (name, Pkg (name,version) size depends confls)

class FromText a where
  fromText :: Text -> a
  fromString :: String -> a
  fromString a = fromText (T.pack a)

instance FromText Version where
  fromText "" = []
  fromText str = map unwrap $ T.splitOn "." str where unwrap x = case decimal x of Right (n,_) -> n

instance FromText PkgId where
  fromText inp = case T.span pat .  T.filter (/= '|') $ inp of
      (name,rest) -> case T.uncons rest of
        Just ('=',vers) -> (name,fromText vers)
        Nothing -> error "unexpected package id format."
    where pat c = isAlphaNum c || (c == '-') || (c == '.') || (c == '+')

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
