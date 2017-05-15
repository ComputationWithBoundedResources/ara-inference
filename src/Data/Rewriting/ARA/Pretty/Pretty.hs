-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Sep 17 09:05:42 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon May  8 10:22:21 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 419
-- URL:
-- Doc URL:
-- Keywords:
-- Compatibility:
--
--

-- Commentary:
--
--
--
--

-- Change Log:
--
--
--

--
--

-- Code:

{-# LANGUAGE CPP #-}

-- | TODO: comment this module
module Data.Rewriting.ARA.Pretty.Pretty
    ( prettyAraDatatype
    , prettyAraSignature
    , prettyAraProblem
    , prettyDtTuple
    , prettyRl
    , prettyTerm
    )
    where
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.Typed.Datatype
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule                               hiding (prettyRule)
import           Data.Rewriting.Typed.Signature

import           Debug.Trace                                             (trace)

import           Control.Exception                                       (throw)
import           Data.List                                               (find, intersperse)
import           Data.Maybe                                              (fromMaybe)
import           Text.PrettyPrint
import qualified Text.PrettyPrint.ANSI.Leijen                            as L

printWhen' :: Bool -> Doc -> (Doc -> Doc)
printWhen' False _ = (empty <>)
printWhen' True  p = (p $+$ empty $+$ )
infixr 5 `printWhen'`


prettyAraProblem :: (Show f, Show v, Show s, Show sDt, Show dt, Show cn) =>
                    ProblemSig f v s sDt dt cn -> Doc
prettyAraProblem prob =
    printWhen' (sterms /= AllTerms) (block "STARTTERM" $ text "CONSTRUCTOR-BASED")
    $ printWhen' (strat /= Full) (block "STRATEGY" $ ppStrat strat)
    $ maybeblock "THEORY" theory ppTheories
    $+$ empty $+$ block "VAR" (ppTexts $ variables prob)
    $+$ empty $+$ maybeblock "DATATYPES" datatypes ppDatatypes
    $+$ empty $+$ maybeblock "SIGNATURES" signatures ppSigs
    $+$ empty $+$ block "RULES" (ppRules $ rules prob)
    $+$ empty $+$ maybeblock "COMMENT" comment text

  where block n pp = parens $ hang empty 3 $ text n $+$ empty $+$ pp
        maybeblock n f fpp = case f prob of
                               Just e  -> block n (fpp e)
                               Nothing -> empty

        ppStrat Innermost = text "INNERMOST"
        ppStrat Outermost = text "OUTERMOST"
        ppStrat Full      = error "Should not be possible."

        ppTexts vs = fsep [ text (show v) | v <- vs]

        ppTheories thys = vcat [ppThy thy | thy <- thys]
            where ppThy (SymbolProperty p fs) = block p (fsep [ text (show f) | f <- fs ])
                  ppThy (Equations rs)        = block "EQUATIONS" $ vcat [ppRule "==" r | r <- rs]

        ppRules rp = vcat ([ppRule "->" r | r <- strictRules rp]
                           ++ [ppRule "->=" r | r <- weakRules rp])

        ppRule = prettyRule

        ppDatatypes dts' = vcat [ ppDatatype dt | dt <- dts' ]
        ppDatatype = prettyAraDatatype ppCost ppCost
        ppCost = const empty

        ppSigs sigs        = vcat
#ifdef DEBUG
                             [ ppSig sig | sig <- sigs ]
#else
                             [ ppSig sig | sig <- filter ((\(_,_,r,_) -> not r) . lhsRootSym) sigs ]
#endif
        ppSig     = prettyAraSignature (text . show) ppCost
          (\(a,b) -> text (show a)) -- <> text ":" <> hcat (intersperse (text ",") $
                                                 -- map ppCost b))

        dts = fromMaybe [] (datatypes prob)

        sterms = startTerms prob
        strat  = strategy prob
        thry   = theory prob


prettyAraDatatype :: (Show dt, Show cn) =>
                     (a -> Doc) -> (b -> Doc) -> Datatype (dt, [a]) (cn, b) -> Doc
prettyAraDatatype pA pB (Datatype (n, cst) chld) =
  hang empty 2 $ text (show n) <> params <+> text "=" <+>
   text (if isRecursive chld then "µX.<" else "<")
   <+> prettyList' (prettyAraCtr pA pB) chld <+> text ">"
    where
          isRecursive = any (\(Constructor _ ch) -> any (\ctrCh -> case ctrCh of
                                                                   ConstructorRecursive -> True
                                                                   _ -> False
                                                                   ) ch)
          params =  if null (show txt)
                       then empty
                       else parens txt
                     where txt = prettyList' pA cst

prettyDtTuple :: (a -> Doc) -> (String, [a]) -> Doc
prettyDtTuple pCst (n, cst) =
  if null cst then empty else text n <> parens (prettyList' pCst cst)


prettyAraCtr :: (Show dt, Show cn) =>
                (a -> Doc) -> (b -> Doc) -> Constructor (dt, [a]) (cn, b) -> Doc
prettyAraCtr pA pB (Constructor (cn, cst) []) =
    text (show cn) <>  csts
      where csts = if null (show txt)
                      then empty
                      else text ":" <> txt
                    where txt = pB cst

prettyAraCtr pA pB (Constructor (cn,cst) chlds) =
    text (show cn) <> ctrTxt <> cstTxt
    where ctrTxt = if null (show txt)
                   then empty
                   else parens txt
                   where txt = prettyList' (prettyAraCtrChld pA) chlds
          cstTxt = if null (show txt)
                      then empty
                      else text ":" <> txt
                   where txt = pB cst


prettyAraCtrChld :: (Show cn) => (a -> Doc) -> ConstructorChild (cn, [a]) -> Doc
prettyAraCtrChld _ ConstructorRecursive          = text "X"
prettyAraCtrChld _ (ConstructorDatatype (dt, _)) = text (show dt)


prettyAraSignature :: (f -> Doc) -> (a -> Doc) -> (b -> Doc) -> Signature (f, a,c,d) b -> Doc
prettyAraSignature pF pCst pDt (Signature (n, cst, _,_) lhs' rhs') =
  hang empty 2 $ pF n <+> text "::" <+>
  pLhs <+> text "-" <> pCst cst <> text "->" <+> pDt rhs'
      where
        pLhs = brackets (hcat $ intersperse (text " x ") (map pDt  lhs'))


prettyList'     :: (a -> Doc) -> [a] -> Doc
prettyList' f l = hcat $ intersperse (comma <> space) (map f l)


prettyRule :: (Show f, Show v) => String -> Rule f v -> Doc
prettyRule sep (Rule lhs rhs) =
  prettyTerm lhs <+> text sep <+> prettyTerm rhs


prettyRl :: (Show f, Show v) => Bool -> Rule f v -> Doc
prettyRl weak (Rule lhs rhs) =
  prettyTerm lhs <+> text (if weak then "->=" else "->") <+> prettyTerm rhs

prettyTerm :: (Show f, Show v) => Term f v -> Doc
prettyTerm (Var v) = text (show v)
prettyTerm (Fun f ch) =
  text (show f) <> char '(' <> hcat (map prettyTerm ch) <> char ')'


--
-- Pretty.hs ends here
