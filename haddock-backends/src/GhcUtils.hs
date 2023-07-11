{-# LANGUAGE BangPatterns, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.GhcUtils
-- Copyright   :  (c) David Waern 2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utils for dealing with types from the GHC API
-----------------------------------------------------------------------------
module GhcUtils ( moduleString,
      mkEmptySigType,
      addClassContext,
      reparenSigType,
      unL,
      reL )where

import Types( XRecCond )

import GHC
    ( Module,
      moduleName,
      Name,
      LSig,
      Sig(TypeSig, ClassOpSig),
      GenLocated(..),
      LHsSigType,
      EpAnn,
      GhcRn,
      UnXRec(unXRec),
      TyClDecl,
      HsTyVarBndr(..),
      HsSigType(..),
      HsType(..),
      LHsType,
      HsForAllTelescope(XHsForAllTelescope, HsForAllVis, HsForAllInvis,
                        hsf_invis_bndrs),
      NoGhcTc,
      ConDeclField(XConDeclField, ConDeclField),
      LHsQTyVars,
      LexicalFixity(Prefix),
      LHsTypeArg,
      HsArg(HsValArg),
      XParTy,
      AnnParen,
      MapXRec(mapXRec),
      WrapXRec(wrapXRec),
      HsOuterTyVarBndrs(..),
      NamedThing(getName),
      moduleNameString,
      tcdName,
      hsLTyVarName,
      mkEmptyWildCardBndrs,
      nlHsTyConApp,
      noAnn,
      noLocA,
      noExtField,
      hsQTvExplicit )

import GHC.Types.Basic ( PromotionFlag(NotPromoted) )

moduleString :: Module -> String
moduleString = moduleNameString . moduleName


mkEmptySigType :: LHsType GhcRn -> LHsSigType GhcRn
-- Dubious, because the implicit binders are empty even
-- though the type might have free variables
mkEmptySigType lty@(L loc ty) = L loc $ case ty of
  HsForAllTy { hst_tele = HsForAllInvis { hsf_invis_bndrs = bndrs }
             , hst_body = body }
    -> HsSig { sig_ext = noExtField
             , sig_bndrs = HsOuterExplicit { hso_xexplicit = noExtField
                                           , hso_bndrs     = bndrs }
             , sig_body = body }
  _ -> HsSig { sig_ext   = noExtField
             , sig_bndrs = HsOuterImplicit{hso_ximplicit = []}
             , sig_body = lty }


addClassContext :: Name -> LHsQTyVars GhcRn -> LSig GhcRn -> LSig GhcRn
-- Add the class context to a class-op signature
addClassContext cls tvs0 (L pos (ClassOpSig _ _ lname ltype))
  = L pos (TypeSig noAnn lname (mkEmptyWildCardBndrs (go_sig_ty ltype)))
  where
    go_sig_ty (L loc (HsSig { sig_bndrs = bndrs, sig_body = ty }))
       = L loc (HsSig { sig_ext = noExtField
                      , sig_bndrs = bndrs, sig_body = go_ty ty })

    go_ty (L loc (HsForAllTy { hst_tele = tele, hst_body = ty }))
       = L loc (HsForAllTy { hst_xforall = noExtField
                           , hst_tele = tele, hst_body = go_ty ty })
    go_ty (L loc (HsQualTy { hst_ctxt = ctxt, hst_body = ty }))
       = L loc (HsQualTy { hst_xqual = noExtField
                         , hst_ctxt = add_ctxt ctxt, hst_body = ty })
    go_ty (L loc ty)
       = L loc (HsQualTy { hst_xqual = noExtField
                         , hst_ctxt = add_ctxt (noLocA []), hst_body = L loc ty })

    extra_pred = nlHsTyConApp NotPromoted Prefix cls (lHsQTyVarsToTypes tvs0)

    add_ctxt (L loc preds) = L loc (extra_pred : preds)

addClassContext _ _ sig = sig   -- E.g. a MinimalSig is fine

lHsQTyVarsToTypes :: LHsQTyVars GhcRn -> [LHsTypeArg GhcRn]
lHsQTyVarsToTypes tvs
  = [ HsValArg $ noLocA (HsTyVar noAnn NotPromoted (noLocA (hsLTyVarName tv)))
    | tv <- hsQTvExplicit tvs ]


--------------------------------------------------------------------------------
-- * Making abstract declarations
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * Parenthesization
-------------------------------------------------------------------------------

-- | Precedence level (inside the 'HsType' AST).
data Precedence
  = PREC_TOP  -- ^ precedence of 'type' production in GHC's parser

  | PREC_SIG  -- ^ explicit type signature

  | PREC_CTX  -- ^ Used for single contexts, eg. ctx => type
              -- (as opposed to (ctx1, ctx2) => type)

  | PREC_FUN  -- ^ precedence of 'btype' production in GHC's parser
              -- (used for LH arg of (->))

  | PREC_OP   -- ^ arg of any infix operator
              -- (we don't keep have fixity info)

  | PREC_CON  -- ^ arg of type application: always parenthesize unless atomic
  deriving (Eq, Ord)

-- | Add in extra 'HsParTy' where needed to ensure that what would be printed
-- out using 'ppr' has enough parentheses to be re-parsed properly.
--
-- We cannot add parens that may be required by fixities because we do not have
-- any fixity information to work with in the first place :(.
reparenTypePrec :: forall a. (XRecCond a)
                => Precedence -> HsType a -> HsType a
reparenTypePrec = go
  where
  -- Shorter name for 'reparenType'
  go :: XParTy a ~ EpAnn AnnParen => Precedence -> HsType a -> HsType a
  go _ (HsBangTy x b ty)     = HsBangTy x b (reparenLType ty)
  go _ (HsTupleTy x con tys) = HsTupleTy x con (map reparenLType tys)
  go _ (HsSumTy x tys)       = HsSumTy x (map reparenLType tys)
  go _ (HsListTy x ty)       = HsListTy x (reparenLType ty)
  go _ (HsRecTy x flds)      = HsRecTy x (map (mapXRec @a reparenConDeclField) flds)
  go p (HsDocTy x ty d)      = HsDocTy x (goL p ty) d
  go _ (HsExplicitListTy x p tys) = HsExplicitListTy x p (map reparenLType tys)
  go _ (HsExplicitTupleTy x tys) = HsExplicitTupleTy x (map reparenLType tys)
  go p (HsKindSig x ty kind)
    = paren p PREC_SIG $ HsKindSig x (goL PREC_SIG ty) (goL PREC_SIG kind)
  go p (HsIParamTy x n ty)
    = paren p PREC_SIG $ HsIParamTy x n (reparenLType ty)
  go p (HsForAllTy x tele ty)
    = paren p PREC_CTX $ HsForAllTy x (reparenHsForAllTelescope tele) (reparenLType ty)
  go p (HsQualTy x ctxt ty)
    = let p' [_] = PREC_CTX
          p' _   = PREC_TOP -- parens will get added anyways later...
          ctxt' = mapXRec @a (\xs -> map (goL (p' xs)) xs) ctxt
      in paren p PREC_CTX $ HsQualTy x ctxt' (goL PREC_TOP ty)
    -- = paren p PREC_FUN $ HsQualTy x (fmap (mapXRec @a (map reparenLType)) ctxt) (reparenLType ty)
  go p (HsFunTy x w ty1 ty2)
    = paren p PREC_FUN $ HsFunTy x w (goL PREC_FUN ty1) (goL PREC_TOP ty2)
  go p (HsAppTy x fun_ty arg_ty)
    = paren p PREC_CON $ HsAppTy x (goL PREC_FUN fun_ty) (goL PREC_CON arg_ty)
  go p (HsAppKindTy x fun_ty arg_ki)
    = paren p PREC_CON $ HsAppKindTy x (goL PREC_FUN fun_ty) (goL PREC_CON arg_ki)
  go p (HsOpTy x prom ty1 op ty2)
    = paren p PREC_FUN $ HsOpTy x prom (goL PREC_OP ty1) op (goL PREC_OP ty2)
  go p (HsParTy _ t) = unXRec @a $ goL p t -- pretend the paren doesn't exist - it will be added back if needed
  go _ t@HsTyVar{} = t
  go _ t@HsStarTy{} = t
  go _ t@HsSpliceTy{} = t
  go _ t@HsTyLit{} = t
  go _ t@HsWildCardTy{} = t
  go _ t@XHsType{} = t

  -- Located variant of 'go'
  goL :: XParTy a ~ EpAnn AnnParen => Precedence -> LHsType a -> LHsType a
  goL ctxt_prec = mapXRec @a (go ctxt_prec) -- should be able to use fmap here

  -- Optionally wrap a type in parens
  paren :: XParTy a ~ EpAnn AnnParen
        => Precedence            -- Precedence of context
        -> Precedence            -- Precedence of top-level operator
        -> HsType a -> HsType a  -- Wrap in parens if (ctxt >= op)
  paren ctxt_prec op_prec | ctxt_prec >= op_prec = HsParTy noAnn . wrapXRec @a
                          | otherwise            = id


-- | Add parenthesis around the types in a 'HsType' (see 'reparenTypePrec')
reparenType :: XRecCond a => HsType a -> HsType a
reparenType = reparenTypePrec PREC_TOP

-- | Add parenthesis around the types in a 'LHsType' (see 'reparenTypePrec')
reparenLType :: forall a. (XRecCond a) => LHsType a -> LHsType a
reparenLType = mapXRec @a reparenType

-- | Add parentheses around the types in an 'HsSigType' (see 'reparenTypePrec')
reparenSigType :: forall a. ( XRecCond a )
               => HsSigType a -> HsSigType a
reparenSigType (HsSig x bndrs body) =
  HsSig x (reparenOuterTyVarBndrs bndrs) (reparenLType body)
reparenSigType v@XHsSigType{} = v

-- | Add parentheses around the types in an 'HsOuterTyVarBndrs' (see 'reparenTypePrec')
reparenOuterTyVarBndrs :: forall flag a. ( XRecCond a )
                       => HsOuterTyVarBndrs flag a -> HsOuterTyVarBndrs flag a
reparenOuterTyVarBndrs imp@HsOuterImplicit{} = imp
reparenOuterTyVarBndrs (HsOuterExplicit x exp_bndrs) =
  HsOuterExplicit x (map (mapXRec @(NoGhcTc a) reparenTyVar) exp_bndrs)
reparenOuterTyVarBndrs v@XHsOuterTyVarBndrs{} = v

-- | Add parentheses around the types in an 'HsForAllTelescope' (see 'reparenTypePrec')
reparenHsForAllTelescope :: forall a. (XRecCond a )
                         => HsForAllTelescope a -> HsForAllTelescope a
reparenHsForAllTelescope (HsForAllVis x bndrs) =
  HsForAllVis x (map (mapXRec @a reparenTyVar) bndrs)
reparenHsForAllTelescope (HsForAllInvis x bndrs) =
  HsForAllInvis x (map (mapXRec @a reparenTyVar) bndrs)
reparenHsForAllTelescope v@XHsForAllTelescope{} = v

-- | Add parenthesis around the types in a 'HsTyVarBndr' (see 'reparenTypePrec')
reparenTyVar :: (XRecCond a) => HsTyVarBndr flag a -> HsTyVarBndr flag a
reparenTyVar (UserTyVar x flag n) = UserTyVar x flag n
reparenTyVar (KindedTyVar x flag n kind) = KindedTyVar x flag n (reparenLType kind)
reparenTyVar v@XTyVarBndr{} = v

-- | Add parenthesis around the types in a 'ConDeclField' (see 'reparenTypePrec')
reparenConDeclField :: (XRecCond a) => ConDeclField a -> ConDeclField a
reparenConDeclField (ConDeclField x n t d) = ConDeclField x n (reparenLType t) d
reparenConDeclField c@XConDeclField{} = c


-------------------------------------------------------------------------------
-- * Located
-------------------------------------------------------------------------------

unL :: GenLocated l a -> a
unL (L _ x) = x

reL :: a -> GenLocated l a
reL = L undefined
-------------------------------------------------------------------------------
-- * NamedThing instances
-------------------------------------------------------------------------------


instance NamedThing (TyClDecl GhcRn) where
  getName = tcdName

