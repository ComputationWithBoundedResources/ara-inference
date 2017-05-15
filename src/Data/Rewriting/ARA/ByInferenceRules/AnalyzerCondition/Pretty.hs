-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Oct  6 23:23:50 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun May  7 22:31:15 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 149
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
--
--

-- Code:

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition.Pretty
    ( prettyACondition
    , prettyACostCondition
    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Pretty ()
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.Operator.Pretty

import           Data.List
import           Text.PrettyPrint


prettyACondition :: (Show b) =>
                    (ACostCondition a -> Doc)
                 -> (ADatatype String b -> Doc)
                 -> ACondition f v a b -> Doc
prettyACondition pC pD (ACondition cost dt share min1Vars) =
  vcat (map (prettyTriple pCs prettyComparison pCs) cost) $+$
  vcat (map (prettyTriple pD prettyComparison pList) share) $+$
  vcat (map (prettyTriple pList prettyComparison pList) dt) $+$
  vcat (map (\(a,b) -> pC b) min1Vars)
  where pCs = hcat . intersperse (text " + ") . map pC
        pList = hcat . intersperse (text " + ") . map pD


prettyTriple :: (a -> Doc) -> (b -> Doc) -> (c -> Doc) -> (a, b, c) -> Doc
prettyTriple pA pB pC (a, b, c) = pA a <+> pB b <+> pC c

prettyACostCondition                             :: (a -> Doc) -> ACostCondition a -> Doc
prettyACostCondition _  (AVariableCondition str) = text str
prettyACostCondition _  (SigRefCst nr)           = text "k" <> parens (int nr)
prettyACostCondition _  (SigRefCstCf nr)         = text "k_cf" <> parens (int nr)
prettyACostCondition pA (ACostValue nr)          = pA nr

--
-- Pretty.hs ends here
