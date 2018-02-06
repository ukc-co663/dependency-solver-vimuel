{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}

module Constraints where

-- import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Monoid
import Data.List (intersperse)


{-| [+A,-B,+C] would be Conj [Require "A", Not (Require "B"), Require "C"] -}
data Constraint a
  = F                                 -- ^ trivially false constraint
  | T                                 -- ^ trivially true constraint
  | Require a                         -- ^ assert that installed (A > 0)
  | Depends a a                       -- ^ left depends on right to be installed first (A < B)
  | Conflicts a a                     -- ^ left requires right to be uninstalled first (-A > B)
  | If (Constraint a) (Constraint a)  -- ^ left implies right ((A > 0) => (A < B))
  | Conj [Constraint a]               -- ^ conjunction of constraints, NB: simplify empty to F
  | Disj [Constraint a]               -- ^ disjunction of constraints, NB: simplify empty to F
  | Not (Constraint a)                -- ^ invert a constraint (not (A > 0))
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | E.g. "A=1.2"
type Output = Text
newtype PkgId = PkgId Output deriving (Eq, Show)

class ToSmtLib a where
  toSmtLib :: a -> Output

instance ToSmtLib PkgId where
  toSmtLib (PkgId x) = "|" <> x <> "|"

instance ToSmtLib a => ToSmtLib (Constraint a) where
  toSmtLib = \case
    F -> "false"
    T -> "true"
    Require x -> "(> " <> toSmtLib x <> " 0)"
    Depends x y -> "(<" <> toSmtLib x <> " " <> toSmtLib y <> ")"
    Conflicts x y -> "(> (- " <> toSmtLib x <> ") " <> toSmtLib y <> ")"
    If c1 c2 -> "(=> (" <> toSmtLib c1 <> ") (" <> toSmtLib c2 <> "))"
    Conj cs -> "(and " <> (mconcat . intersperse " " . map toSmtLib) cs <> ")"
    Disj cs -> mconcat ["(or ",(mconcat . intersperse " " . map toSmtLib) cs,")"]
    Not x -> mconcat ["(not ",toSmtLib x,")"]

{-|
>>> simplify $ Conj [Require "A"]
Require "A"

>>> simplify $ Not (Conj [])
Not Empty

NB: Use 'simplifyDeep' to put into simplest form.
>>> simplify $ Disj [Conj [Disj [Conj [Disj [], Disj [F]]], Disj []], Disj [Depends "A" "B", Depends "A" "C"]]
Disj [Disj [Depends "A" "B",Depends "A" "C"]]
-}
simplify :: Eq a => Constraint a -> Constraint a
simplify c =
  case c of
    Require a -> Require a
    Depends a b -> Depends a b
    Conflicts a b -> Conflicts a b
    Conj [] -> F
    Disj [] -> F
    Conj [c] -> simplify c
    Disj [c] -> simplify c
    Conj cs -> let cs' = filter (/= T) $ map simplify cs in if F `elem` cs' then F else Conj cs' -- TODO enforces Eq constraint on 'a :(
    Disj cs -> let cs' = filter (/= F) $ map simplify cs in if T `elem` cs' then T else Disj cs'
    Not (Not c) -> simplify c
    Not F -> T
    Not T -> F
    If F _ -> T
    If T c -> simplify c
    If c1 c2 -> If (simplify c1) (simplify c2)
    Not c -> Not $ simplify c
    T -> T
    F -> F

{-|
>>> simplifyDeep $ Disj [Conj [Disj [Conj [Disj [], Disj [F]]], Disj []], Disj [Depends "A" "B", Depends "A" "C"]]
Disj [Depends "A" "B",Depends "A" "C"]
-}
simplifyDeep :: Eq a => Constraint a -> Constraint a
simplifyDeep = flog simplify where flog f x = if f x == x then x else flog f (f x)

pkgs :: Constraint a -> [a] -- or Set a?
pkgs = foldMap return

fold :: (b -> a -> b) -> b -> Constraint a -> b
fold = foldl'

{- |
>>> pkgs' $ Disj [Depends "A" "B", Depends "A" "C"]
fromList ["A","B","C"]
-}
pkgs' :: Ord a => Constraint a -> Set a
pkgs' = foldl' (flip Set.insert) Set.empty

{-
The target state is a conjunction of constraints, e.g.: ["+A","-B>2","+C=4"]:

    let target = Conj [Disj [Require "A=1.0",Require "A=1.1"],Not (Disj [Require "B=3",Require "B=4"]),Require "C=4"]

then get the pkgs referred to and add them to some global stack of unprocessed variables

    >>> pkgs target
    ["A=1.0","A=1.1","B=3","B=4","C=4"] -- TODO what about negative constraints? We shouldn't need to add dependency information (although it can hardly harm Z3)

While there are unprocessed packages:

    1. Get package meta
    2. Make a constraint of the form
      `If (Require "X") (Conj [Conj [Disj [Depends "X" "A1",Depends "X" "A2",...], Disj [dep1b,...]],Conj [Conflicts "X" "C1",Conflicts "X" "C2",...]])`
      (add any new packages to list of unprocessed packages)
    3. Add current package to set of already processed packages
    4. Repeat with next unprocessed package

    data PkgMeta = PkgMeta
      { refString :: Text
      , version :: Version
      , size :: Integer
      , depends :: [[PkgConstr]]
      , conflicts :: [PkgConstr]
      , installed :: Bool
-}

pkgConstraint :: a -> Constraint a -> Constraint a -> Constraint a
pkgConstraint p deps confls = If (Require p) (Conj [deps,confls])

pkgDepends :: a -> [[a]] -> Constraint a
pkgDepends p = Conj . map (Disj . map (Depends p))

pkgConflicts :: a -> [a] -> Constraint a
pkgConflicts p = Conj . map (Conflicts p)


-- pkgDepends :: a -> [[[a]]] -> Constraint a
-- pkgDepends p = Conj . map (Conj . map (Disj . map (Depends p)))
--
-- pkgConflicts :: a -> [[a]] -> Constraint a
-- pkgConflicts p = Conj . map (Conj . map (Conflicts p))
