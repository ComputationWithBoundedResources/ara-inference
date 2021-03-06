{-# LANGUAGE CPP #-}
-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Sep  9 15:15:02 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon Jul 23 10:26:17 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 381
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

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.Prove.Pretty
    ( prettyProve
    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.InfTreeNode.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.Prove.Type
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Pretty
import           Data.Rewriting.ARA.Pretty
import           Data.Rewriting.Typed.Problem
import           Prelude                                                      hiding ((<>))
import qualified Text.PrettyPrint.ANSI.Leijen                                 as L


import           Data.List                                                    (intersperse)
import           Data.Maybe                                                   (fromMaybe)
import           Prelude                                                      hiding
                                                                               ((<$>),
                                                                               (<>))
import           Text.PrettyPrint
#define DEBUG


line = text "" $+$ empty

prettyProve :: (Show f, Show v, Show dt, Show s, Show cn, Show sDt) =>
               Prove f v s sDt dt cn -> Doc
prettyProve prove =
  hang empty 2 $
  text "InfTreeNodes To Prove:" $+$ line $+$
  vcat (intersperse line (map prettyInfTreeNode (infTreeNodesToProve prove))) $+$ line $+$
  text "Proven InfTreeNodes: " $+$line $+$
  vcat (intersperse line (map prettyInfTreeNode (provenInfTreeNodes prove))) $+$ line $+$
  text "Signature Map: " $+$line $+$
  vcat (zipWith (\nr (y, n, z) -> int nr <> text ": "<>
                               prettyAraSignature (text . show) pCst pDt y
#ifdef DEBUG
                               <+> text "\tFromRule: " <> int n
                               <+> text "\t" <> text z
#endif
                ) ([0..] :: [Int])
        (signatureMap prove)) $+$ line $+$
  text "Cost Free Signature Map: " $+$line $+$
  vcat (zipWith (\nr (y,n,z) -> int nr <> text ": "<>
                        prettyAraSignature (text . show) pCst pDt y
#ifdef DEBUG
                        <+> text "\tGroup: " <> int n
                        <+> text "\tFrom: " <> text z
#endif
                ) ([0..] :: [Int])
        (costFreeSigs prove)) $+$ line $+$
  text "Constraints:" $+$ line $+$
  prettyCond (conditions prove) $+$ line $+$
  text "Problem:" $+$ line $+$
  prettyAraProblem (problem prove) $+$
  text "Next Variable Number: " <+> int (varNr prove) $+$ line
  where pCst = prettyACost prettyVector
        pDt  = prettyADatatype pCst

prettyCond = prettyACondition (prettyACostCondition int) (prettyADatatype (prettyACost int))

--
-- Pretty.hs ends here
