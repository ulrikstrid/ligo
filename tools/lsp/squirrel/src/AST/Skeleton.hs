{- | The AST and auxiliary types along with their pretty-printers.

     The comments for fields in types are the type before it was made untyped.
-}

{-# LANGUAGE DeriveGeneric #-}

module AST.Skeleton where

import Control.Lens.Lens (Lens, lens)
import Data.Text (Text)

import Duplo.Pretty (Pretty (..))
import Duplo.Tree

import Data.Functor.Classes
import GHC.Generics (Generic)
import Product

data SomeLIGO xs where
  SomeLIGO :: Lang -> LIGO xs -> SomeLIGO xs

nestedLIGO :: Lens (SomeLIGO xs) (SomeLIGO xs') (LIGO xs) (LIGO xs')
nestedLIGO = lens getLIGO setLIGO
  where
    getLIGO (SomeLIGO _ ligo) = ligo
    setLIGO (SomeLIGO d _) = SomeLIGO d

withNestedLIGO
  :: Functor f => SomeLIGO xs -> (LIGO xs -> f (LIGO xs')) -> f (SomeLIGO xs')
withNestedLIGO = flip nestedLIGO

instance Pretty (LIGO xs) => Pretty (SomeLIGO xs) where
  pp (SomeLIGO _ nested) = pp nested


-- | The AST for Pascali... wait. It is, em, universal one.
--
--   TODO: Rename; add stuff if CamlLIGO/ReasonLIGO needs something.
--
-- type LIGO        = Tree RawLigoList
type LIGO xs = Tree RawLigoList (Product xs)
type Tree' fs xs = Tree fs (Product xs)

type RawLigoList =
  [ Name, QualifiedName, Pattern, RecordFieldPattern, Constant, FieldAssignment
  , MapBinding, Alt, Expr, TField, Variant, Type, Binding
  , RawContract, TypeName, FieldName, MichelsonCode
  , Error, Ctor, Contract, NameDecl, Preprocessor, PreprocessorCommand
  , NameModule, ModuleAccess
  ]

data Lang
  = Pascal
  | Caml
  | Reason
  deriving stock Show

-- Let 'Accessor' be either 'FieldName' or a 'Text'ual representation of an
-- index (a number).

data Contract it
  = ContractEnd
  | ContractCons it it -- ^ Declaration
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

newtype RawContract it
  = RawContract [it] -- ^ Declaration
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveFor1List

data Binding it
  = BFunction     IsRec it [it] (Maybe it) it -- ^ (Name) (Parameters) (Type) (Expr)
  | BParameter it it -- ^ (Name) (Type)
  | BVar          it (Maybe it) (Maybe it) -- ^ (Name) (Type) (Expr)
  | BConst        it (Maybe it) (Maybe it) -- ^ (Name) (Type) (Expr)
  | BTypeDecl     it it -- ^ (Name) (Type)
  | BAttribute    it -- ^ (Name)
  | BInclude      it
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

type IsRec = Bool

data Type it
  = TArrow    it it    -- ^ (Type) (Type)
  | TRecord   [it]     -- ^ [TField]
  | TSum      [it]     -- ^ [Variant]
  | TProduct  [it]     -- ^ [Type]
  | TApply    it [it]  -- ^ (Name) [Type]
  | TString   Text     -- ^ (TString)
  | TOr       it it it it
  | TAnd      it it it it
  | TWildcard
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data Variant it
  = Variant it (Maybe it)  -- (Name) (Maybe (Type))
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data TField it
  = TField it it  -- (Name) (Type)
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

-- | TODO: break onto smaller types? Literals -> Constant; mapOps; mmove Annots to Decls.
data Expr it
  = Let       it it   -- Declaration Expr
  | Apply     it [it] -- (Expr) [Expr]
  | Constant  it -- (Constant)
  | Ident     it -- (QualifiedName)
  | BinOp     it it it -- (Expr) Text (Expr)
  | UnOp      it it -- (Expr)
  | Op        Text
  | Record    [it] -- [Assignment]
  | If        it it (Maybe it) -- (Expr) (Expr) (Expr)
  | Assign    it it -- (LHS) (Expr)
  | List      [it] -- [Expr]
  | ListAccess it [it] -- (Name) [Indexes]
  | Set       [it] -- [Expr]
  | Tuple     [it] -- [Expr]
  | Annot     it it -- (Expr) (Type)
  | Attrs     [it]
  | BigMap    [it] -- [MapBinding]
  | Map       [it] -- [MapBinding]
  | MapRemove it it -- (Expr) (QualifiedName)
  | SetRemove it it -- (Expr) (QualifiedName)
  | Case      it [it]                  -- (Expr) [Alt]
  | Skip
  | ForLoop   it it it (Maybe it) it              -- (Name) (Expr) (Expr) (Expr)
  | WhileLoop it it                    -- (Expr) (Expr)
  | Seq       [it]                     -- [Declaration]
  | Block     [it]                     -- [Declaration]
  | Lambda    [it] (Maybe it) it               -- [VarDecl] (Maybe (Type)) (Expr)
  | ForBox    it (Maybe it) it it it -- (Name) (Maybe (Name)) Text (Expr) (Expr)
  | MapPatch  it [it] -- (QualifiedName) [MapBinding]
  | SetPatch  it [it] -- (QualifiedName) [Expr]
  | RecordUpd it [it] -- (QualifiedName) [FieldAssignment]
  | Michelson it it [it] -- (MichelsonCode) (Type) (Arguments)
  | Paren     it -- (Expr)
  deriving stock (Generic, Functor, Foldable, Traversable)

newtype MichelsonCode it
  = MichelsonCode Text
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype Preprocessor it
  = Preprocessor it -- (PreprocessorCommand)
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveFor1Field

data Alt it
  = Alt it it -- (Pattern) (Expr)
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data MapBinding it
  = MapBinding it it -- (Expr) (Expr)
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data FieldAssignment it
  = FieldAssignment [it] it -- [Accessor] (Expr)
  | Spread it -- (Name)
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data Constant it
  = Int     Text
  | Nat     Text
  | String  Text
  | Float   Text
  | Bytes   Text
  | Tez     Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data Pattern it
  = IsConstr     it (Maybe it) -- (Name) (Maybe (Pattern))
  | IsConstant   it -- (Constant)
  | IsVar        it -- (Name)
  | IsCons       it it -- (Pattern) (Pattern)
  | IsAnnot      it it -- (Pattern) (Type) -- Semantically `Var`
  | IsWildcard
  | IsSpread     it   -- (Name)
  | IsList       [it] -- [Pattern]
  | IsTuple      [it] -- [Pattern]
  | IsRecord     [it] -- [FieldPattern]
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

-- Used specifically in record destructuring
data RecordFieldPattern it
  = IsRecordField it it
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data ModuleAccess it = ModuleAccess
  { maPath  :: [it] -- [Name]
  , maField :: it -- Accessor
  }
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data QualifiedName it
  = QualifiedName
    { qnSource ::  it -- Name
    , qnPath   :: [it] -- [Accessor]
    }
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

newtype Name it = Name
  { _raw     :: Text
  }
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype NameDecl it = NameDecl
  { _raw     :: Text
  }
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype NameModule it = NameModule Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype TypeName it = TypeName Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype Ctor it = Ctor Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype PreprocessorCommand it = PreprocessorCommand Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype FieldName it = FieldName Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

data Error it = Error Text [it]
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

-- TODO: is used also for comparing nodes in which the order
-- of elements is unimportant (for instance sum types) so
-- they may be mistreated as unequal.
liftEqList :: (a -> b -> Bool) -> [a] -> [b] -> Bool
liftEqList f a b = all (==True) $ uncurry f <$> zip a b

-- TODO
-- class GEq1 f where
--   gLiftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

-- class Eq1' (f :: * -> *) where
--   liftEq' :: (a -> b -> Bool) -> f a -> f b -> Bool
--   default
--     liftEq'
--       :: ( Generic (f a), Generic (f b)
--          , GEq1 (G.Rep (f a)), GEq1 (G.Rep (f b))
--          )
--       => (a -> b -> Bool) -> f a -> f b -> Bool
--   liftEq' f a b = gLiftEq f (G.from a) (G.from b)

-- newtype T a = T a deriving stock Generic

-- instance Eq1 T where
--   liftEq = liftEq'

newtype DefaultEq1DeriveForText it =
  DefaultEq1DeriveForText { unDefaultEq1DeriveForText :: Text }

instance Eq1 DefaultEq1DeriveForText where
  liftEq _ (DefaultEq1DeriveForText a) (DefaultEq1DeriveForText b) = a == b

newtype DefaultEq1DeriveFor1Field it =
  DefaultEq1DeriveFor1Field { unDefaultEq1DeriveFor1Field :: it }

instance Eq1 DefaultEq1DeriveFor1Field where
  liftEq f (DefaultEq1DeriveFor1Field a) (DefaultEq1DeriveFor1Field b) = f a b

data DefaultEq1DeriveFor2Field it =
  DefaultEq1DeriveFor2Field
    { unDefaultEq1DeriveFor2FieldA :: it
    , unDefaultEq1DeriveFor2FieldB :: it
    }

instance Eq1 DefaultEq1DeriveFor2Field where
  liftEq f (DefaultEq1DeriveFor2Field a b) (DefaultEq1DeriveFor2Field c d) = f a c && f b d

newtype DefaultEq1DeriveFor1List it =
  DefaultEq1DeriveFor1List { unDefaultEq1DeriveFor1List :: [it] }

instance Eq1 DefaultEq1DeriveFor1List where
  liftEq f (DefaultEq1DeriveFor1List a) (DefaultEq1DeriveFor1List b) = liftEqList f a b

--------------------------------------------------------------------------------

instance Eq1 Alt where
  liftEq f (Alt pa ea) (Alt pb eb) = f pa pb && f ea eb

instance Eq1 MapBinding where
  liftEq f (MapBinding a b) (MapBinding c d) = f a c && f b d

instance Eq1 RecordFieldPattern where
  liftEq f (IsRecordField la ba) (IsRecordField lb bb) = f la lb && f ba bb

instance Eq1 FieldAssignment where
  liftEq f (FieldAssignment as a) (FieldAssignment bs b) =
    liftEqList f as bs && f a b
  liftEq f (Spread a) (Spread b) = f a b
  liftEq _ _ _ = False

instance Eq1 Constant where
  liftEq _ (Int a) (Int b) = a == b
  liftEq _ (Nat a) (Nat b) = a == b
  liftEq _ (String a) (String b) = a == b
  liftEq _ (Float a) (Float b) = a == b
  liftEq _ (Bytes a) (Bytes b) = a == b
  liftEq _ (Tez a) (Tez b) = a == b
  liftEq _ _ _ = False

instance Eq1 Expr where
  liftEq f (Constant a) (Constant b) = f a b
  liftEq f (Ident a) (Ident b) = f a b
  liftEq f (List as) (List bs) = liftEqList f as bs
  liftEq f (Tuple as) (Tuple bs) = liftEqList f as bs
  liftEq f (Annot ea ta) (Annot eb tb) = f ea eb && f ta tb
  liftEq f (Set xs) (Set ys) = liftEqList f xs ys
  liftEq f (Map xs) (Map ys) = liftEqList f xs ys
  liftEq f (BigMap xs) (BigMap ys) = liftEqList f xs ys
  liftEq _ _ _ = False

instance Eq1 Contract where
  liftEq f (ContractCons a as) (ContractCons b bs) = f a b && f as bs
  liftEq _ ContractEnd ContractEnd = True
  liftEq _ _ _ = False

instance Eq1 Error where
  -- liftEq _ _ _ = error "Cannot compare `Error` nodes"
  liftEq _ _ _ = False

instance Eq1 Binding where
  -- liftEq _ _ _ = error "Cannot compare `Binding`"
  liftEq _ _ _ = False

instance Eq1 Type where
  liftEq f (TArrow a b) (TArrow c d) = f a c && f b d
  liftEq f (TRecord xs) (TRecord ys) = liftEqList f xs ys
  liftEq f (TSum xs) (TSum ys) = liftEqList f xs ys
  liftEq f (TProduct xs) (TProduct ys) = liftEqList f xs ys
  liftEq _ (TString xs) (TString ys) = xs == ys
  liftEq _ _ _ = False

instance Eq1 Variant where
  liftEq f (Variant an (Just at)) (Variant bn (Just bt)) = f an bn && f at bt
  liftEq f (Variant an Nothing) (Variant bn Nothing) = f an bn
  liftEq _ _ _ = False

instance Eq1 TField where
  liftEq f (TField an at) (TField bn bt) = f an bn && f at bt

instance Eq1 ModuleAccess where
  liftEq f (ModuleAccess ap asrc) (ModuleAccess bp bsrc) =
    f asrc bsrc && liftEqList f ap bp

instance Eq1 QualifiedName where
  liftEq f (QualifiedName asrc ap) (QualifiedName bsrc bp) =
    f asrc bsrc && liftEqList f ap bp

instance Eq1 Pattern where
  liftEq f (IsConstr na mbpa) (IsConstr nb mbpb) =
    f na nb && case (mbpa, mbpb) of
      (Just a, Just b) -> f a b
      (_, _) -> False
  liftEq f (IsConstant a) (IsConstant b) = f a b
  liftEq f (IsVar a) (IsVar b) = f a b
  liftEq f (IsCons ha ta) (IsCons hb tb) = f ha hb && f ta tb
  liftEq f (IsAnnot pa ta) (IsAnnot pb tb) = f pa pb && f ta tb
  liftEq _ IsWildcard IsWildcard = True
  liftEq f (IsSpread a) (IsSpread b) = f a b
  liftEq f (IsList xa) (IsList xb) = liftEqList f xa xb
  liftEq f (IsTuple xa) (IsTuple xb) = liftEqList f xa xb
  liftEq _ _ _ = False
