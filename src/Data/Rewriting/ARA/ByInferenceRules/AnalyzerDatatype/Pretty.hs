-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Oct  1 15:44:01 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon Jul 23 10:24:11 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 81
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
module Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Pretty
    ( prettyADatatype

    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Prelude                                                   hiding ((<>))
import           Text.PrettyPrint

prettyADatatype :: (Show dt, Show a) => (ACost a -> Doc) -> ADatatype dt a -> Doc
prettyADatatype pCst (ActualCost fromCf dt cst) =
  text (show dt) <> (if fromCf then text "_cf" else empty) <> pCst cst

prettyADatatype _ x = text (show x)


--
-- Pretty.hs ends here


