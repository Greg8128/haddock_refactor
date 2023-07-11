{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances    #-} 

-- 'new' Ghctypes to replace the old one. --
module GhcTypes2 where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import GHC.Exts (Symbol)
import GHC.Utils.Outputable
    ( SDoc, Outputable(..), OutputableBndr )

-- the "import list" below contains all imports used by the Hoogle backend.

-- import GHC
--   ( AnnParen, -- possibly unused.
--     ClsInst, -- Typeclass interface. only used via Outputable. can capture via wrapper.
--     ConDecl -- keep in full (done)
--       ( ConDeclGADT,
--         ConDeclH98,
--         con_args,
--         con_bndrs,
--         con_g_args,
--         con_mb_cxt,
--         con_name,
--         con_names,
--         con_res_ty
--       ),
--     ConDeclField (ConDeclField, XConDeclField, cd_fld_names, cd_fld_type), -- should be kept
--     DynFlags (pprCols), -- should be kept
--     EpAnn, -- odl
--     FamilyDecl (FamilyDecl, fdInfo, fdLName),
--     FamilyInfo (ClosedTypeFamily, OpenTypeFamily),
--     FieldOcc (foExt),
--     FixitySig (FixitySig),
--     ForeignDecl (ForeignExport, ForeignImport),
--     GenLocated (..),
--     GhcPass,
--     GhcRn,
--     HsArg (HsValArg), -- should likely be copied "as-is"
--     HsArrow (HsUnrestrictedArrow), -- should be reimplemented, at least with HsUnrestrictedArrow
--     HsConDeclGADTDetails (PrefixConGADT, RecConGADT), -- only used in ppCtor
--     HsConDetails (InfixCon, PrefixCon, RecCon), -- a simple structure. should likely keep as-is
--     HsDataDefn (dd_cons, dd_derivs), -- only used in ppData to clear relevant fields.
--     HsDecl (ForD, SigD, TyClD), -- used in ppExport, but pretty small.
--     HsForAllTelescope
--       ( HsForAllInvis,
--         HsForAllVis,
--         XHsForAllTelescope,
--         hsf_invis_bndrs
--       ),
--     HsOuterTyVarBndrs (..),
--     HsSigType (..),
--     HsTyVarBndr (..),
--     HsType (..), -- is constructed AND used, so may need to be represented in full.
--     IdP, -- a type family resolving to IdGhcP in practice.

--        -- several type synonyms for an XRec wrapper
--     LFamilyDecl,
--     LHsDecl,
--     LHsQTyVars (),
--     LHsSigType,
--     LHsType,
--     LHsTypeArg,
--     LSig,

--     LexicalFixity (Prefix), -- Prefix  | Infix
--     LocatedA, -- GenLocated SrcSpanAnnA
--     LocatedN, -- GenLocated SrcSpanAnnN
--     MapXRec (..),
--     Module,  -- used via moduleName
--     ModuleName, -- used via moduleName
--     Name, -- hopefully shouldn't be needed.
--     NamedThing (getName), -- typeclass
--     NoGhcTc, -- A type family over passes. Could be redundant, as it's used in XRecCond
--     OutputableBndrId, -- complex type constraint
--     RdrName, -- only Outputable is needed.
--     Sig (ClassOpSig, PatSynSig, TypeSig),
--     TyClDecl (ClassDecl, DataDecl, FamDecl, SynDecl, tcdATDefs, tcdATs, tcdDataDefn, tcdMeths, tcdSigs),
--     UnXRec (..), -- class to remove XRec
--     WrapXRec (..), -- class to add a trivial XRec
--     XParTy, -- pass parentheses type.
--     alwaysQualify, -- can be imported
--     dropWildCards, -- c=???
--     emptyLHsBinds, --
--     getConNames,
--     hsLTyVarName,
--     hsLinear,
--     hsQTvExplicit,
--     hsScaledThing,
--     mkEmptyWildCardBndrs,
--     moduleName,
--     moduleNameString,
--     n2l,
--     nlHsTyConApp,
--     noAnn,
--     noExtField,
--     noHsUniTok,
--     noLocA,
--     pprTyFamInstDecl,
--     tcdName,
--     tyClDeclTyVars,
--     unLoc,
--   )

--------------------------------------------------------------------------------

-- * utilities such as existential wrappers

--------------------------------------------------------------------------------

data AnOutputable = forall a. (Outputable a) => AnOutputable a

instance Outputable AnOutputable where
  ppr :: AnOutputable -> SDoc
  ppr (AnOutputable a) = ppr a

-- names. How do we implement them?

data NoExtField = NoExtField

noExtField :: NoExtField
noExtField = NoExtField

data SourceText = SourceText String | NoSourceText

type family NoGhcTcPass (p :: Pass) :: Pass
type instance NoGhcTcPass p = p

class IsPass p where
  ghcPass :: GhcPass p
  
instance IsPass 'Renamed where
  ghcPass :: GhcPass 'Renamed
  ghcPass = GhcRn

data TokenLocation = NoTokenLoc

data HsToken (tok :: Symbol) = HsTok
type LHsToken tok p = XRec p (HsToken tok)

data HsLinearArrowTokens pass
  = HsPct1 !(LHsToken "%1" pass) !(LHsUniToken "->" "→" pass)
  | HsLolly !(LHsToken "⊸" pass)

data Specificity = InferredSpec | SpecifiedSpec

class CanReparen a where
  reparen :: a -> a

data ReparenOut = forall a. (CanReparen a, Outputable a) => ReparenOut a

instance Outputable ReparenOut where
  ppr (ReparenOut a) = ppr a

instance CanReparen ReparenOut where
  reparen (ReparenOut a) = ReparenOut (reparen a)
--------------------------------------------------------------------------------

-- * the types

--------------------------------------------------------------------------------
data AnnParen = AnnParen

type ClsInst = AnOutputable

-- | Constructor Declaration
--   Got rid of XConDecl because it supposedly should not happen.
data ConDecl pass
  = ConDeclGADT {con_names :: NonEmpty (LIdP pass), con_bndrs :: XRec pass (HsOuterSigTyVarBndrs pass), con_mb_cxt :: Maybe (LHsContext pass), con_res_ty :: LHsType pass, con_g_args :: HsConDeclGADTDetails pass}
  | ConDeclH98 {con_name :: LIdP pass, con_mb_cxt :: Maybe (LHsContext pass), con_args :: HsConDeclH98Details pass}

data ConDeclField pass = ConDeclField

data FamilyDecl pass = FamilyDecl {fdInfo :: FamilyInfo, fdLName :: LIdP pass}

-- Hoogle treats closed type families as open type families.
data FamilyInfo = DataFamily | OpenTypeFamily

data FixitySig pass = FixitySig NoExtField [LIdP pass] Fixity

-- Fixity, from GHC.Types.Fixity . Do we even do anything with it?
data Fixity = Fixity SourceText Int FixityDirection

data FixityDirection = InfixL | InfixN | InfixR

data ForeignDecl pass = ForeignImport NoExtField (LIdP pass) (LHsSigType pass) NoExtField
                      | ForeignExport  NoExtField (LIdP pass) (LHsSigType pass) NoExtField

-- | the GenLocated type.
-- TODO add a lot of instances
data GenLocated l e = L l e


data HsArg tm ty
  = HsValArg tm
  | HsTypeArg SrcSpan ty
  | HsArgPar SrcSpan

-- note - newer versions of GHC have introduced new types of arrows
data HsArrow pass = HsUnrestrictedArrow !(LHsUniToken "->" "→" pass)
                  | HsLinearArrow !(HsLinearArrowTokens pass)


-- TODO add a helper function to extract the list
data HsConDeclGADTDetails pass = HsConDeclGADTDetails [LBangType pass]

data HsConDetails tyarg arg rec
  = PrefixCon [tyarg] [arg]
  | RecCon rec
  | InfixCon arg arg


-- used in PPExport 
data HsDecl p = ForD (TyClDecl p)
              | TyClD ()
              | SigD ()
              | OtherD

type HsForallTelescope p = ReparenOut

type HsOuterTyVarBndrs f a = ReparenOut

type HsTyVarBndr f p = ReparenOut

data HsUniToken (tok :: Symbol) (utok :: Symbol) = HsNormalTok | HsUnicodeTok

class Outputable a => IsHsSigType a where
  dropHsDocTy :: a -> a

data AnHsSigType = forall a. IsHsSigType a => AnHsSigType a

instance Outputable AnHsSigType where
  ppr (AnHsSigType a) = ppr a

instance IsHsSigType AnHsSigType where
  dropHsDocTy (AnHsSigType a) = AnHsSigType (dropHsDocTy a)

type HsSigType p = AnHsSigType

type OutputableBndrId pass = (OutputableBndr (IdGhcP pass), OutputableBndr (IdGhcP (NoGhcTcPass pass)), Outputable (GenLocated (Anno (IdGhcP pass)) (IdGhcP pass)), Outputable (GenLocated (Anno (IdGhcP (NoGhcTcPass pass))) (IdGhcP (NoGhcTcPass pass))), IsPass pass)


--     LFamilyDecl,
--     LHsDecl,
--     LHsQTyVars (),
--     LHsSigType,
--     LHsType,
--     LHsTypeArg,
--     LSig,

type LFamilyDecl pass = XRec pass (FamilyDecl pass)
type LHsDecl p = XRec p (HsDecl p)
type LHsQTyVars p = [LHsTypeArg p]
type LHsSigType p = XRec p (HsSigType p)
type LHsTypeArg p = AnOutputable
type LHsTyVarBndr flag pass = XRec pass (HsTyVarBndr flag pass)
type LHsUniToken tok utok p = XRec p (HsUniToken tok utok)


newtype Module = Module String

newtype ModuleName = ModuleName String

type RdrName = AnOutputable


-- move this into another module because stuff like ppData depends on Types.hs
-- and we do NOT want circular dependencies.

data TyClDecl pass = FamDecl () (FamilyDecl pass)
                   | SynDecl --
                   | DataDecl --
                   | ClassDecl () --



-- this is commented out because it resutls in a type error at the moment
-- hsLinear = HsScaled (HsLinearArrow (HsPct1 noHsTok noHsUniTok))

moduleName :: Module -> String
moduleName (Module a) = a
moduleNameString :: ModuleName -> String
moduleNameString (ModuleName a) = a


noHsTok :: GenLocated TokenLocation (HsToken tok)
noHsTok = L NoTokenLoc HsTok
noHsUniTok :: GenLocated TokenLocation (HsUniToken tok utok)

noHsUniTok = L NoTokenLoc HsNormalTok



---- below are some definitions from the 'old' Ghctypes -----

-- | Lexical fixity
data LexicalFixity = Prefix | Infix

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

data XFixitySig p = XS ()

type LIdP p = XRec p (IdP p)

-- type family XRec p a = r | r -> a

-- type instance XRec (GhcPass p) a = GenLocated (Anno a) a

type XRec p a = GenLocated (Anno a) a

type family IdP p

type instance IdP (GhcPass p) = IdGhcP p

-- placeholder values. will be filled in if needed.
type family IdGhcP pass where
  IdGhcP 'Renamed = Name

data Name = Name

data GhcPass (c :: Pass) where
  GhcRn :: GhcPass 'Renamed

-- The three passes. My guess is that only "renamed" is actually used
data Pass = Renamed
  deriving (Data)

data Anno a = Anno {placeholder :: ()}


data HsDataDefn pass = HsDataDefn {dd_cons :: [LConDecl pass], dd_ND :: NewOrData}

data LConDecl pass = XRec pass (ConDecl pass)

data NewOrData = NewType | DataType

--       ConDecl(ConDeclH98, ConDeclGADT, con_name, con_names, con_bndrs,
--               con_mb_cxt, con_res_ty, con_g_args, con_args),

type LSig pass = XRec pass (Sig pass)

data Sig pass = PatSynSig | TypeSig | Other

type HsOuterSigTyVarBndrs pass = () --  HsOuterTyVarBndrs Specificity

data HsConDeclH98Details pass = HsConDeclH98Details {placeholder :: ()}

type LHsContext pass = XRec pass (HsContext pass)

type HsContext pass = [LHsType pass]

type LHsType pass = XRec pass (HsType pass)

type LConDeclField pass = XRec pass (ConDeclField pass)

data HsScaled pass a = HsScaled (HsArrow pass) a

-- -- | HsLinearArrow !(HsLinearArrowTokens pass)
-- -- | HsExplicitMult !(LHsToken "%" pass) !(LHsType pass) !(LHsUniToken "->" "→" pass)

type LBangType pass = XRec pass (BangType pass)

type BangType pass = HsType pass

data HsType pass = HsType {placeholder :: ()}


