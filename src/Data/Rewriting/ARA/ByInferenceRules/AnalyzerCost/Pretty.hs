-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Oct  1 15:55:48 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Dec 20 21:56:08 2016 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 89
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
module Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Pretty
    ( prettyACost

    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Type

import           Text.PrettyPrint

prettyACost                       :: (a -> Doc) -> ACost a -> Doc
prettyACost pACst (ACost cst) = pACst cst


--
-- Pretty.hs ends here
