{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- Note [Pass sensitive types]
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Types
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskellorg
-- Stability   :  experimental
-- Portability :  portable
--
-- Types that are commonly used through-out Haddock. Some of the most
-- important types are defined here, like 'Interface' and 'DocName'.
module Types
  ( module Documentation.Haddock.Types,
    Fixity (..),
    XRecCond,
    Doc,
    DocForDecl,
    DocMarkup,
    DocMarkupH
      ( Markup,
        markupAName,
        markupAppend,
        markupBold,
        markupCodeBlock,
        markupDefList,
        markupEmphasis,
        markupEmpty,
        markupExample,
        markupHeader,
        markupHyperlink,
        markupIdentifier,
        markupIdentifierUnchecked,
        markupMathDisplay,
        markupMathInline,
        markupModule,
        markupMonospaced,
        markupOrderedList,
        markupParagraph,
        markupPic,
        markupProperty,
        markupString,
        markupTable,
        markupUnorderedList,
        markupWarning
      ),
    DocOption (OptHide),
    Documentation (Documentation),
    ExportItem
      ( ExportDecl,
        expItemDecl,
        expItemFixities,
        expItemMbDoc,
        expItemPats,
        expItemSubDocs,
        ExportOther
      ),
    Header (Header),
    Hyperlink (Hyperlink),
    Interface
      ( ifaceDoc,
        ifaceExportItems,
        ifaceInstances,
        ifaceMod,
        ifaceOptions
      ),
    MDoc,
    MetaDoc (_doc),
    ModLink (ModLink),
    Table (Table),
    exampleToString,
    showWrapped,
    -- $
    runWriter,
    tell,
  )
where

import Control.DeepSeq (NFData (..), deepseq)
-- import Control.Exception (throw)
import Control.Monad.Catch
  ( Exception (),
    SomeException,
  )
import Control.Monad.Writer.Strict (MonadWriter (..), runWriter)
import Data.Data (Data)
import Data.Map (Map)
import Data.Typeable (Typeable)
import Documentation.Haddock.Types
-- import GHC.Types.Basic (PromotionFlag(..))

import GHC
  ( AnnParen,
    ClsInst,
    EpAnn,
    GhcPass,
    GhcRn,
    HsDecl,
    HsType (),
    IdP,
    LHsDecl,
    LHsQTyVars (),
    MapXRec (..),
    Module,
    ModuleName,
    Name,
    NamedThing (getName),
    NoGhcTc,
    OutputableBndrId,
    Sig,
    TyClDecl,
    UnXRec (..),
    WrapXRec (..),
    XParTy,
  )
import GHC.Types.Fixity (Fixity (..))
-- import GHC.Driver.Session (Language)
-- import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.Name.Occurrence (HasOccName (..), OccName)
import GHC.Utils.Outputable
  ( Outputable (..),
    OutputableBndr (pprBndr, pprInfixOcc, pprPrefixOcc),
    char,
    hcat,
    text,
    (<+>),
  )

-----------------------------------------------------------------------------

-- * Interface

-----------------------------------------------------------------------------

-- | 'Interface' holds all information used to render a single Haddock page.
-- It represents the /interface/ of a module. The core business of Haddock
-- lies in creating this structure. Note that the record contains some fields
-- that are only used to create the final record, and that are not used by the
-- backends.
data Interface = Interface
  { -- | The module behind this interface.
    ifaceMod :: !Module,
    ifaceDoc :: !(Documentation Name),
    --   -- | Documentation header with cross-reference information.
    -- , ifaceRnDoc           :: !(Documentation DocName)

    -- | Haddock options for this module (prune, ignore-exports, etc).
    ifaceOptions :: ![DocOption],
    --   -- | Documentation of declarations originating from the module (including
    --   -- subordinates).

    ifaceExportItems :: ![ExportItem GhcRn],
    --   -- | Instances exported by the module.
    ifaceInstances :: ![ClsInst]
  }

-----------------------------------------------------------------------------

-- * Export items & declarations

-----------------------------------------------------------------------------

data ExportItem name
  = -- | An exported declaration.
    ExportDecl
      { -- | A declaration.
        expItemDecl :: !(LHsDecl name),
        -- | Bundled patterns for a data type declaration
        expItemPats :: ![(HsDecl name, DocForDecl (IdP name))],
        -- | Maybe a doc comment, and possibly docs for arguments (if this
        -- decl is a function or type-synonym).
        expItemMbDoc :: !(DocForDecl (IdP name)),
        -- | Subordinate names, possibly with documentation.
        expItemSubDocs :: ![(IdP name, DocForDecl (IdP name))],
        -- \| Fixity decls relevant to this declaration (including subordinates).

        -- | Instances relevant to this declaration, possibly with
        -- documentation.
        -- , expItemInstances :: ![DocInstance name]
        expItemFixities :: ![(IdP name, Fixity)]
      }
  | -- | Any other type of export item.
    ExportOther

data Documentation name = Documentation
  { documentationDoc :: Maybe (MDoc name),
    documentationWarning :: !(Maybe (Doc name))
  }
  deriving (Functor)

-- | Arguments and result are indexed by Int, zero-based from the left,
-- because that's the easiest to use when recursing over types.
type FnArgsDoc name = Map Int (MDoc name)

type DocForDecl name = (Documentation name, FnArgsDoc name)

-- noDocForDecl :: DocForDecl name
-- noDocForDecl = (Documentation Nothing Nothing, mempty)

-----------------------------------------------------------------------------

-- * Cross-referencing

-----------------------------------------------------------------------------

-- | Extends 'Name' with cross-reference information.
data DocName
  = -- | This thing is part of the (existing or resulting)
    -- documentation. The 'Module' is the preferred place
    -- in the documentation to refer to.
    Documented Name Module
  | -- | This thing is not part of the (existing or resulting)
    -- documentation, as far as Haddock knows.
    Undocumented Name
  deriving (Eq, Data)

-- data DocNameI

-- type instance NoGhcTc DocNameI = DocNameI

-- type instance IdP DocNameI = DocName

-- instance CollectPass DocNameI where
--   collectXXPat _ ext = dataConCantHappen ext
--   collectXXHsBindsLR ext = dataConCantHappen ext

instance NamedThing DocName where
  getName (Documented name _) = name
  getName (Undocumented name) = name

-- | Useful for debugging
instance Outputable DocName where
  ppr = ppr . getName

instance OutputableBndr DocName where
  pprBndr _ = ppr . getName
  pprPrefixOcc = pprPrefixOcc . getName
  pprInfixOcc = pprInfixOcc . getName

-- class (NamedThing name) => SetName name where
--   setName :: Name -> name -> name

-- instance SetName Name where
--   setName name' _ = name'

-- instance SetName DocName where
--   setName name' (Documented _ mdl) = Documented name' mdl
--   setName name' (Undocumented _) = Undocumented name'

-- | Adds extra "wrapper" information to a name.
--
-- This is to work around the fact that most name types in GHC ('Name', 'RdrName',
-- 'OccName', ...) don't include backticks or parens.
data Wrap n
  = -- | don't do anything to the name
    Unadorned {unwrap :: n}
  | -- | add parentheses around the name
    Parenthesized {unwrap :: n}
  | -- | add backticks around the name
    Backticked {unwrap :: n}
  deriving (Show, Functor, Foldable, Traversable)

-- | Useful for debugging
instance (Outputable n) => Outputable (Wrap n) where
  ppr (Unadorned n) = ppr n
  ppr (Parenthesized n) = hcat [char '(', ppr n, char ')']
  ppr (Backticked n) = hcat [char '`', ppr n, char '`']

showWrapped :: (a -> String) -> Wrap a -> String
showWrapped f (Unadorned n) = f n
showWrapped f (Parenthesized n) = "(" ++ f n ++ ")"
showWrapped f (Backticked n) = "`" ++ f n ++ "`"

instance HasOccName DocName where
  occName = occName . getName

-----------------------------------------------------------------------------

-- * Instances

-----------------------------------------------------------------------------

-- | The three types of instances
data InstType name
  = ClassInst
      { clsiCtx :: [HsType name],
        clsiTyVars :: LHsQTyVars name,
        clsiSigs :: [Sig name],
        clsiAssocTys :: [PseudoFamilyDecl name]
      }
  | -- | Body (right-hand side)
    TypeInst (Maybe (HsType name))
  | -- | Data constructors
    DataInst (TyClDecl name)

instance
  (OutputableBndrId p) =>
  Outputable (InstType (GhcPass p))
  where
  ppr (ClassInst {..}) =
    text "ClassInst"
      <+> ppr clsiCtx
      <+> ppr clsiTyVars
      <+> ppr clsiSigs
  ppr (TypeInst a) = text "TypeInst" <+> ppr a
  ppr (DataInst a) = text "DataInst" <+> ppr a

-- | Almost the same as 'FamilyDecl' except for type binders.
--
-- In order to perform type specialization for class instances, we need to
-- substitute class variables to appropriate type. However, type variables in
-- associated type are specified using 'LHsTyVarBndrs' instead of 'HsType'.
-- This makes type substitution impossible and to overcome this issue,
-- 'PseudoFamilyDecl' type is introduced.
data PseudoFamilyDecl name

-- | An instance origin information.
--
-- This is used primarily in HTML backend to generate unique instance
-- identifiers (for expandable sections).
data InstOrigin name
  = OriginClass name
  | OriginData name
  | OriginFamily name

instance (NamedThing name) => NamedThing (InstOrigin name) where
  getName (OriginClass name) = getName name
  getName (OriginData name) = getName name
  getName (OriginFamily name) = getName name

-----------------------------------------------------------------------------

-- * Documentation comments

-----------------------------------------------------------------------------

-- type LDoc id = Located (Doc id)

type Doc id = DocH (Wrap (ModuleName, OccName)) (Wrap id)

type MDoc id = MetaDoc (Wrap (ModuleName, OccName)) (Wrap id)

type DocMarkup id a = DocMarkupH (Wrap (ModuleName, OccName)) id a

instance
  (NFData a, NFData mod) =>
  NFData (DocH mod a)
  where
  rnf doc = case doc of
    DocEmpty -> ()
    DocAppend a b -> a `deepseq` b `deepseq` ()
    DocString a -> a `deepseq` ()
    DocParagraph a -> a `deepseq` ()
    DocIdentifier a -> a `deepseq` ()
    DocIdentifierUnchecked a -> a `deepseq` ()
    DocModule a -> a `deepseq` ()
    DocWarning a -> a `deepseq` ()
    DocEmphasis a -> a `deepseq` ()
    DocBold a -> a `deepseq` ()
    DocMonospaced a -> a `deepseq` ()
    DocUnorderedList a -> a `deepseq` ()
    DocOrderedList a -> a `deepseq` ()
    DocDefList a -> a `deepseq` ()
    DocCodeBlock a -> a `deepseq` ()
    DocHyperlink a -> a `deepseq` ()
    DocPic a -> a `deepseq` ()
    DocMathInline a -> a `deepseq` ()
    DocMathDisplay a -> a `deepseq` ()
    DocAName a -> a `deepseq` ()
    DocProperty a -> a `deepseq` ()
    DocExamples a -> a `deepseq` ()
    DocHeader a -> a `deepseq` ()
    DocTable a -> a `deepseq` ()

#if !MIN_VERSION_ghc(8,0,2)
-- These were added to GHC itself in 8.0.2
instance NFData Name where rnf x = seq x ()
instance NFData OccName where rnf x = seq x ()
instance NFData ModuleName where rnf x = seq x ()
#endif

instance (NFData id) => NFData (Header id) where
  rnf (Header a b) = a `deepseq` b `deepseq` ()

instance (NFData id) => NFData (Hyperlink id) where
  rnf (Hyperlink a b) = a `deepseq` b `deepseq` ()

instance (NFData id) => NFData (ModLink id) where
  rnf (ModLink a b) = a `deepseq` b `deepseq` ()

instance NFData Picture where
  rnf (Picture a b) = a `deepseq` b `deepseq` ()

instance NFData Example where
  rnf (Example a b) = a `deepseq` b `deepseq` ()

instance (NFData id) => NFData (Table id) where
  rnf (Table h b) = h `deepseq` b `deepseq` ()

instance (NFData id) => NFData (TableRow id) where
  rnf (TableRow cs) = cs `deepseq` ()

instance (NFData id) => NFData (TableCell id) where
  rnf (TableCell i j c) = i `deepseq` j `deepseq` c `deepseq` ()

exampleToString :: Example -> String
exampleToString (Example expression result) =
  ">>> " ++ expression ++ "\n" ++ unlines result

-----------------------------------------------------------------------------

-- * Options

-----------------------------------------------------------------------------

-- | Source-level options for controlling the documentation.
data DocOption
  = -- | This module should not appear in the docs.
    OptHide
  | OptPrune
  | -- | Pretend everything is exported.
    OptIgnoreExports
  | -- | Not the best place to get docs for things
    -- exported by this module.
    OptNotHome
  | -- | Render enabled extensions for this module.
    OptShowExtensions
  deriving (Eq, Show)

-----------------------------------------------------------------------------

-- A monad which collects error messages, locally defined to avoid a dep on mtl

-- type ErrMsgM = Writer [ErrMsg]

-- Exceptions

-- | Haddock's own exception type.
data HaddockException
  = HaddockException String
  | WithContext [String] Control.Monad.Catch.SomeException
  deriving (Typeable)

instance Show HaddockException where
  show (HaddockException str) = str
  show (WithContext ctxts se) = unlines $ ["While " ++ ctxt ++ ":\n" | ctxt <- reverse ctxts] ++ [show se]

instance Control.Monad.Catch.Exception HaddockException

-----------------------------------------------------------------------------

-- * Pass sensitive types

-----------------------------------------------------------------------------
type XRecCond a =
  ( XParTy a ~ EpAnn AnnParen,
    NoGhcTc a ~ a,
    MapXRec a,
    UnXRec a,
    WrapXRec a (HsType a)
  )
 
