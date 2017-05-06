-- TypeSignatures.hs ---
--
-- Filename: TypeSignatures.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Oct  1 16:02:50 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sat May  6 19:11:06 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 92
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
module Data.Rewriting.ARA.ByInferenceRules.TypeSignatures where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.Typed.Datatype
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Signature

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Ops

import           Text.PrettyPrint


type ASignatureSig = Signature (String, ACost Vector, Bool, Bool) (ADatatype Vector)
type SignatureSig = Signature (String, ACost Int, Bool,Bool) (String, [ACost Int])
type ProblemSig f v s sDt dt cn  = Problem f v (s, ACost Int, Bool,Bool)
                   (sDt,[ACost Int]) (dt, [ACost Int]) (cn, ACost Int)

type DatatypeSig = Datatype (String, [ACost Int]) (String, ACost Int)
type ConstructorSig = Constructor (String, [ACost Int]) (String, ACost Int)
type ConstructorChildSig = ConstructorChild (String, [ACost Int])

--
-- TypeSignatures.hs ends here
