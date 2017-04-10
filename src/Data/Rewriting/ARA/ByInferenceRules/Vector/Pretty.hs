-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed May  4 17:34:21 2016 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Wed May  4 20:48:47 2016 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 12
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

module Data.Rewriting.ARA.ByInferenceRules.Vector.Pretty where

import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type

import           Text.PrettyPrint

prettyVector                                :: Vector -> Doc
prettyVector (Vector1 a)                    = parens $ (int a)
prettyVector (Vector2 x1 x2)                = parens $ (int x1) <> comma <+> (int x2)
prettyVector (Vector3 x1 x2 x3 )            = parens $ (int x1) <> comma <+> (int x2) <> comma <+> (int x3)
prettyVector (Vector4 x1 x2 x3 x4 )         = parens $ (int x1) <> comma <+> (int x2) <> comma <+> (int x3) <> comma <+> (int x4)
prettyVector (Vector5 x1 x2 x3 x4 x5)       = parens $ (int x1) <> comma <+> (int x2) <> comma <+> (int x3) <> comma <+> (int x4) <> comma <+> (int x5)
prettyVector (Vector6 x1 x2 x3 x4 x5 x6)    = parens $ (int x1) <> comma <+> (int x2) <> comma <+> (int x3) <> comma <+> (int x4) <> comma <+> (int x5) <> comma <+> (int x6)
prettyVector (Vector7 x1 x2 x3 x4 x5 x6 x7) = parens $ (int x1) <> comma <+> (int x2) <> comma <+> (int x3) <> comma <+> (int x4) <> comma <+> (int x5) <> comma <+> (int x6) <> comma <+> (int x7)


--
-- Pretty.hs ends here
