-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Dec 16 21:14:03 2014 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sat Jun 10 15:19:10 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 17
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
module Data.Rewriting.ARA.ByInferenceRules.Operator.Pretty
    ( prettyOrdering
    , prettyComparison
    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.Operator.Type
import           Text.PrettyPrint

prettyOrdering :: Ordering -> Doc
prettyOrdering GT = text ">="
prettyOrdering LT = text "<="
prettyOrdering EQ = text "=="

prettyComparison     :: Comparison -> Doc
prettyComparison Geq = text ">="
prettyComparison Leq = text "<="
prettyComparison Eq  = text "=="

--
-- Pretty.hs ends here
