-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Oct  6 23:24:12 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Fri Apr 14 19:20:12 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 80
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
module Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition.Type
    ( ACondition (..)
    , ACostCondition (..)
    , toACostConditionVector
    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Ops  ()
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type

import           Data.Rewriting.Typed.Rule


data ACondition a b = ACondition
              { costCondition   :: [([ACostCondition a], Comparison, [ACostCondition a])] -- ^
              , dtConditions    :: [([ADatatype b], Comparison, [ADatatype b])] -- ^
              , shareConditions :: [(ADatatype b, Comparison, [ADatatype b])] -- ^
              , minus1Vars :: [(Rule String String, ACostCondition a)]
              } deriving (Show, Eq)

data ACostCondition a = AVariableCondition String
                      | SigRefCst Int
                      | SigRefCstCf Int
                      | ACostValue a
                      deriving (Eq)

toACostConditionVector :: ACostCondition Int -> ACostCondition Vector
toACostConditionVector (ACostValue x)         = ACostValue (Vector1 x)
toACostConditionVector (SigRefCst x)          = SigRefCst x
toACostConditionVector (SigRefCstCf x)        = SigRefCstCf x
toACostConditionVector (AVariableCondition x) = AVariableCondition x


instance Show a => Show (ACostCondition a) where
  show (AVariableCondition x) = x
  show (SigRefCst nr)         = "k(" ++ show nr ++ ")"
  show (SigRefCstCf nr)       = "k_cf(" ++ show nr ++ ")"
  show (ACostValue nr)        = show nr


--
-- Type.hs ends here
