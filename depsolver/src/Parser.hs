{-# language OverloadedStrings #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}

module Parser
  ( decode
  , module Parser
  )
 where

import Control.Monad (forM_)

import Data.Aeson
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.Char (isAlphaNum, isDigit)
import Data.Monoid ((<>))
import qualified Data.Map.Lazy as MapL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.FilePath ((</>))

import qualified Data.ByteString.Lazy as B
import Data.Foldable (foldl')

import Debug.Trace

type Constraint = [(Action,Reference)]

data Command = Do Action Reference deriving Show
instance ToText Command where
  toText (Do a r) = toText a <> toText r

type State = [Reference]

type Name = Text
data Reference = Ref Name (Maybe (Relation, Version)) deriving Show

instance ToText Reference where
  toText (Ref name (Just (rel, vers))) = name <> toText rel <> toText vers
  toText (Ref name _) = name

toString :: ToText a => a -> String
toString = T.unpack . toText

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

type Size = Word

data PkgMeta = Meta
  { version :: Version
  , size :: Size
  , depends :: [[Reference]]
  , conflicts :: [Reference]
  , inConstraint :: Bool
  } deriving Show

mkRef :: Name -> PkgMeta -> String
mkRef name meta = T.unpack $ name <> "=" <> vers
  where vers = toText $ version meta

type Repo = MapL.Map Name [PkgMeta]

data Action = Add | Remove deriving Show
instance ToText Action where
  toText Add = "+"
  toText Remove = "-"

class ToText a where
  toText :: a -> Text

type PkgState = [Reference]

type Target = [Command]

-- test = do
--   repository <- repository
--   let ?repo = repository in
--     print $ get $ Ref "B" $ Just (Eq, [3,2])

get :: Repo -> Reference -> Maybe (Name, [PkgMeta])
get repo (Ref name vers) =
  case MapL.lookup name repo of -- TODO use monad instance of maybe
    Nothing -> Nothing
    Just pkgs ->
      case vers of
        Nothing ->
          Just (name, pkgs)
        (Just (rel, vers)) ->
          Just (name, filter (\Meta{version = v} -> toOperator rel v vers) pkgs)




repository :: FilePath -> IO Repo
repository wd =  do
  raw <- B.readFile $ wd </> "repository.json"
  return $ parseRepo raw

constraints :: FilePath -> IO Target
constraints wd = do
  raw <- B.readFile $ wd </> "constraints.json"
  let Just cs = decode raw :: Maybe [Text]
  return $ map toCommand cs

initial :: FilePath -> IO [Reference]
initial wd = do
  raw <- B.readFile $ wd </> "initial.json"
  let Just cs = decode raw :: Maybe [Text]
  return $ map toRef cs

-- checkIfInRepo inits repo =
--   forM_ inits (\ref -> case get repo ref of Nothing -> print $ toString ref; _ -> return ())

-- example: given a repo, return its total size
allSizes :: Repo -> Size
allSizes = foldl' (\n Meta{size = s} -> n + s) 0 . concatMap snd . MapL.toList


instance {-# overlaps #-} FromJSON (Name, PkgMeta) where
  parseJSON = withObject "Package" $ \obj -> do
    name <- obj .: "name"
    version <- fmap toVersion $ obj .: "version"
    size <- obj .: "size"
    dep <- obj .:? "depends" .!= []
    let depends = map (map toRef) dep
    -- if (any ("freepats" `T.isPrefixOf`) . concat) dep then traceM (show (name, version, depends)) else return ()
    con <- obj .:? "conflicts" .!= []
    let conflicts = map toRef con
    return (name, Meta version size depends conflicts False)


parseRepo :: B.ByteString -> Repo
parseRepo raw = fmap (sortBy (comparing version)) $ MapL.fromListWith (++) [(k,[v]) | (k,v) <- inp]
  where inp = V.toList $ fromJust $ decode raw

toVersion :: Text -> Version
toVersion "" = []
toVersion str = map unwrap $ T.splitOn "." str
unwrap x = case decimal x of Right (n,_) -> n

toRef :: Text -> Reference
toRef inp = case T.span (\c -> isAlphaNum c || (c == '-') || (c == '.') || (c == '+')) inp of
  (name,"") -> Ref name Nothing
  (name,rest) -> case T.break (\c -> isDigit c || (c == '.')) rest of
    (rel,vers) -> Ref name (Just (toRel rel, toVersion vers))

toRel :: Text -> Relation
toRel inp = case inp of
  "<" -> Lt
  "<=" -> LtEq
  "=" -> Eq
  ">=" -> GtEq
  ">" -> Gt

toCommand :: Text -> Command
toCommand inp = case T.uncons inp of
  Just ('+', rest) -> Do Add (toRef rest)
  Just ('-', rest) -> Do Remove (toRef rest)
