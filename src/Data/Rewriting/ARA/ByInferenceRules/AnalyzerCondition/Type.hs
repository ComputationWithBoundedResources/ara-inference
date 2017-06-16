-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Oct  6 23:24:12 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Fri Jun 16 17:36:45 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 93
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


data ACondition f v a b = ACondition
              { costCondition   :: [([ACostCondition a], Comparison, [ACostCondition a])] -- ^
              , dtConditions    :: [([ADatatype String b], Comparison, [ADatatype String b])] -- ^
              , dtConditionsInt :: [(ADatatype String Int, Comparison, Int)] -- ^
              , shareConditions :: [(ADatatype String b, Comparison, [ADatatype String b])] -- ^
              , minus1Vars :: [(Rule f v, ACostCondition a)]
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
