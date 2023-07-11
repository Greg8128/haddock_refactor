{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Copies of types used by GHC.
-- This allows the backend to not be affected by GHC updates.
module GhcTypes where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Data)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty)
import GHC.Exts (Symbol)

-- | Lexical fixity
data LexicalFixity = Prefix | Infix

-- | Type variable
type TyVar = Var -- Placeholder

data Var = Var {placeholder :: ()}

-- | The GHC monad
-- the Hoogle backend only needs to obtain the dyn flags
data Ghc a = Ghc {unGhc :: Session -> IO a}

class (ExceptionMonad m) => GhcMonad m where
  getSessionDynFlags :: m DynFlags
  setSessionDynFlags :: DynFlags -> m ()

type ExceptionMonad m = (MonadCatch m, MonadThrow m, MonadMask m, MonadIO m)

data Session = Session !(IORef DynFlags)

data DynFlags = DynFlags {pprCols :: Int, dumpDir :: Maybe String, objectDir :: Maybe String, hiDir :: Maybe String, hieDir :: Maybe String, stubDir :: Maybe String, includePaths :: IncludeSpecs}

data IncludeSpecs = IncludeSpecs
  { includePathsQuote :: [String],
    includePathsGlobal :: [String],
    includePathsQuoteImplicit :: [String]
  }
  deriving (Show)

-- | the GenLocated type.
-- TODO add a lot of instances
data GenLocated l e = L l e

type LocatedAn an = GenLocated (SrcAnn an)

type LocatedN = GenLocated SrcSpanAnnN

type SrcAnn ann = SrcSpanAnn' (EpAnn ann)

type SrcSpanAnnN = SrcAnn NameAnn

data NameAnn = NameAnn {placeholder :: ()}

data EpAnn ann
  = EpAnn {placeholder :: ()}
  | EpAnnNotUsed

noAnn :: EpAnn ann
noAnn = EpAnnNotUsed

noSrcSpan :: SrcSpan
noSrcSpan = SrcSpan ()

noLocA :: e -> GenLocated (SrcSpanAnn' (EpAnn ann)) e
noLocA = L (SrcSpanAnn EpAnnNotUsed noSrcSpan)

unLoc :: GenLocated l e -> e
unLoc (L _ a) = a

data SrcSpanAnn' a = SrcSpanAnn {ann :: !a, locA :: !SrcSpan}

data SrcSpan = SrcSpan {placeholder :: ()}

data RealSrcLoc = RealSrcLoc {placeholder :: ()}

data FixitySig pass
  = FixitySig (XFixitySig pass) [LIdP pass] Fixity
  | XFixitySig

-- Fixity, from GHC.Types.Fixity . Do we even do anything with it?
data Fixity = Fixity {p1, p2, p3 :: ()}

data XFixitySig p = XS ()

type LIdP p = XRec p (IdP p)

type family XRec p a = r | r -> a

type instance XRec (GhcPass p) a = GenLocated (Anno a) a

type family IdP p

type instance IdP (GhcPass p) = IdGhcP p

-- placeholder values. will be filled in if needed.
type family IdGhcP pass where
  IdGhcP 'Parsed = ()
  IdGhcP 'Renamed = ()
  IdGhcP 'Typechecked = ()

data GhcPass (c :: Pass) where
  GhcPs :: GhcPass 'Parsed
  GhcRn :: GhcPass 'Renamed
  GhcTc :: GhcPass 'Typechecked

-- The three passes. My guess is that only "renamed" is actually used
data Pass = Parsed | Renamed | Typechecked
  deriving (Data)

data Anno a = Anno {placeholder :: ()}

-- | Type class declaration !!!
data TyClDecl pass = FamDecl {tcdFam :: FamilyDecl pass} | SynDecl | DataDecl {tcdDataDefn :: HsDataDefn pass, tcdLName :: LIdP pass } | ClassDecl {tcdSgs :: [LSig pass]} | XTyClDecl

data HsDataDefn pass = HsDataDefn {dd_cons :: [LConDecl pass], dd_ND :: NewOrData}
data LConDecl pass = XRec pass (ConDecl pass)
data NewOrData = NewType | DataType 
--       ConDecl(ConDeclH98, ConDeclGADT, con_name, con_names, con_bndrs,
--               con_mb_cxt, con_res_ty, con_g_args, con_args),

type LSig pass = XRec pass (Sig pass)
data Sig pass = PatSynSig | TypeSig | Other
-- | Constructor Declaration
--   Got rid of XConDecl because it supposedly should not happen.
data ConDecl pass
  = ConDeclGADT {con_names :: NonEmpty (LIdP pass), con_bndrs :: XRec pass (HsOuterSigTyVarBndrs pass), con_mb_cxt :: Maybe (LHsContext pass), con_res_ty :: LHsType pass, con_g_args :: HsConDeclGADTDetails pass}
  | ConDeclH98 {con_name :: LIdP pass, con_mb_cxt :: Maybe (LHsContext pass), con_args :: HsConDeclH98Details pass}

type HsOuterSigTyVarBndrs pass = () --  HsOuterTyVarBndrs Specificity

data HsConDeclGADTDetails pass
  = PrefixConGADT [HsScaled pass (LBangType pass)]
  | RecConGADT (XRec pass [LConDeclField pass]) (LHsUniToken "->" "→" pass)

data HsConDeclH98Details pass = HsConDeclH98Details {placeholder :: ()}

type LHsContext pass = XRec pass (HsContext pass)

type HsContext pass = [LHsType pass]

type LHsType pass = XRec pass (HsType pass)

type LConDeclField pass = XRec pass (ConDeclField pass)

data ConDeclField pass = ConDeclField

data HsScaled pass a = HsScaled (HsArrow pass) a

data HsArrow pass = HsUnrestrictedArrow !(LHsUniToken "->" "→" pass)

-- -- | HsLinearArrow !(HsLinearArrowTokens pass)
-- -- | HsExplicitMult !(LHsToken "%" pass) !(LHsType pass) !(LHsUniToken "->" "→" pass)

type LBangType pass = XRec pass (BangType pass)

type BangType pass = HsType pass

data HsType pass = HsType {placeholder :: ()}

type XXType pass = Type

data Type = Type {placeholder :: ()}

type LHsUniToken tok utok p = XRec p (HsUniToken tok utok)

data HsUniToken (tok :: Symbol) (utok :: Symbol) = HsNormalTok | HsUnicodeTok

data FamilyDecl pass = FamilyDecl {fdLName :: LIdP pass}
