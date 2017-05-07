-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Oct  6 13:20:53 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun May  7 22:35:58 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 113
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

module Data.Rewriting.ARA.ByInferenceRules.InfTreeNode.Type
    ( InfTreeNode (..)
    , InfTreeNodeView (..)
    , FunSig (..)
    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type

import           Data.Maybe
import           Data.Rewriting.Typed.Term

data InfTreeNode f v dt = InfTreeNode
    { preConditions :: [(String, ADatatype dt Int)] -- ^ e.g. (x, r(0)).
    , costs         :: [ACostCondition Int]         -- ^ costs
    , postCondition :: Maybe (Term f v, ADatatype dt Int) -- ^ the statement
    , functionName  :: (f, String, Bool, [ACostCondition Int], Int, Maybe [(f, Int)])
    -- ^ functionName, isChildInfTreeNode (constructors), cstsOfRoot,
    -- signatureNrOfRoot, isCostFreeDerivationBranch, Maybe idxOfCfSig
    , history       :: [(Int, String, InfTreeNodeView)] -- ^ history of the context
    } deriving (Eq,Show)


data InfTreeNodeView = InfTreeNodeView
                       [(String, ADatatype String Vector)] -- ^ preConditions
                       [ACostCondition Vector]             -- ^ costs
                       (Term String String, ADatatype String Vector) -- ^ postCondition
                     | InfTreeNodeLeafView
                       FunSig         -- ^ cost-full signature
                       (Maybe FunSig) -- ^ cost-free signature
                     | InfTreeNodeLeafEmpty
                       deriving (Eq)

data FunSig = FunSig
              String                    -- ^ function name
              [ADatatype String Vector] -- ^ preConditions
              [ACostCondition Vector]   -- ^ costs
              (ADatatype String Vector) -- ^ postCondition
              deriving (Eq)


-- instance Show InfTreeNode where
--     show (InfTreeNode pre c post _ history') =
--          showListWithSep show pre ", "++ " |-" ++ show c ++
--                              "- " ++ show post ++ "\n\n\t"
--                              ++ showListWithSep show history' "\n\t"

instance Show InfTreeNodeView where
  show InfTreeNodeLeafEmpty = ""
  show (InfTreeNodeView pre c post) =
    showListWithSep show pre ", "++ " |-" ++ show c ++ "- " ++ show post
  show (InfTreeNodeLeafView sig cfSig) =
    printSig sig ++ if isNothing cfSig
                    then ""
                    else "\t" ++ printSig (fromJust cfSig)
    where printSig (FunSig f pre c post) =
            f ++ " :: " ++ showListWithSep show pre " x "++ " -" ++ show c ++
            "-> " ++ show post


showListWithSep              :: Show a => (a -> String) -> [a] -> String -> String
showListWithSep _ [] _       = []
showListWithSep f [x] _      = f x
showListWithSep f (x:xs) sep = f x ++ sep ++ showListWithSep f xs sep


--
-- Type.hs ends here
