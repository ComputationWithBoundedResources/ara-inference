-- TypeSignatures.hs ---
--
-- Filename: TypeSignatures.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Oct  1 16:02:50 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun May  7 18:54:35 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 108
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


type ASignatureSig s sDt = Signature (s, ACost Vector, Bool, Bool) (ADatatype sDt Vector)
type SignatureSig s sDt = Signature (s, ACost Int, Bool,Bool) (sDt, [ACost Int])

type ProblemSig f v s sDt dt cn  = Problem f v (s, ACost Int, Bool,Bool)
                   (sDt,[ACost Int]) (dt, [ACost Int]) (cn, ACost Int)

type DatatypeSig dt cn = Datatype (dt, [ACost Int]) (cn, ACost Int)
type ConstructorSig dt cn  = Constructor (dt, [ACost Int]) (cn, ACost Int)
type ConstructorChildSig dt = ConstructorChild (dt, [ACost Int])

--
-- TypeSignatures.hs ends here
