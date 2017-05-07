-- InfRuleMisc.hs ---
--
-- Filename: InfRuleMisc.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Sep 15 01:47:10 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun May  7 21:40:53 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 426
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

-- Code:

{-# LANGUAGE CPP #-}

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleMisc
    ( getNewVariableName
    , instances
    , getConstructor
    , getRewriteRuleByName
    , getDatatypeByName
    , getConstructorByName
    , getConstructorByName'
    , termName
    , sig2ASig
    , getTermVars
    , fst3
    , snd3
    , thd3
    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments.Type
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions       hiding
                                                                            (getDatatypeByName)
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.Typed.Datatype
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature

#ifdef DEBUG
import           Debug.Trace                                               (trace)
#endif

import           Control.Exception                                         (throw)
import           Data.List                                                 (find)
import           Data.Maybe                                                (fromJust,
                                                                            fromMaybe)
import           Debug.Trace
import           Text.PrettyPrint

-- | This function takes two Integer values as input parameters. The first one
--   is the input parameter for the old value, which needs to be kept track of
--   to ensure the variables are unique. And the second one is the number of
--   variables to generate. The return value is a tuple, containing the new
--   variable counter and the variables.
getNewVariableName :: Int -> Int -> (Int, [String])
getNewVariableName oldV num
  | num < 1 = (oldV, [])
  | otherwise =
    (oldV + num, strs start num)
    where
      start = oldV
      strs      :: Int -> Int -> [String]
      strs nr n = foldl (\acc x -> acc ++ ["ipvar" ++ show x]) [] [nr..nr+n-1]

-- | This function returns the number of same elements in the given list.
--   Input: A element and a list of elements.
instances :: (Eq a) => a -> [a] -> Int
instances _ [] = 0
instances x (y:ys)
    | x==y = 1 + instances x ys
    | otherwise = instances x ys


-- | @getConstructor prob name dtStr@ gets the constructor @name@ from the
-- data-type with the name dtStr. The information is retrieved from the problem
-- @prob@.
getConstructor :: (Eq cn, Eq dt) =>
                  ProblemSig f v s sDt dt cn -> cn -> dt -> Maybe (ConstructorSig dt cn)
getConstructor prob name dtStr = let mDt = getDatatypeByName prob dtStr
                                 in case mDt of
                                      Nothing -> Nothing
                                      Just dt -> find (\(Constructor cn _) -> name == fst cn)
                                                     (constructors dt)

getConstructorByName :: (Eq cn) => ProblemSig f v s sDt dt cn -> cn -> Maybe (ConstructorSig dt cn)
getConstructorByName p n = let dt = concatMap constructors (fromMaybe [] (datatypes p))
                           in find (\(Constructor x _ ) -> fst x == n) dt

getConstructorByName' :: (Eq cn) => [DatatypeSig dt cn] -> cn -> Maybe (ConstructorSig dt cn)
getConstructorByName' dts n = let dt = concatMap constructors dts
                               in find (\(Constructor x _ ) -> fst x == n) dt

-- | @getDatatypeByName prob dt@ checks all data-types defined in the problem
-- @prob@ and searches for the one with the datatype @dt@.
getDatatypeByName :: (Eq dt) => ProblemSig f v s sDt dt cn -> dt -> Maybe (DatatypeSig dt cn)
getDatatypeByName prob dt = getDatatypeWith prob (\(Datatype dtn _) -> dt == fst dtn)


-- | This function searches the rewrite rule in the given TRS by its name.
--   It returns the rewrite rule packed in the monad, or fails.
getRewriteRuleByName :: (Eq f) => ProblemSig f v s sDt dt cn -> f -> Maybe (Rule f v)
getRewriteRuleByName t n = find (\r -> n == rName r) (allRules (rules t))
    where rName rule = rootSymbol (lhs rule)

          rootSymbol :: Term f v -> f
          rootSymbol (Var v)   = error "root symbol cannot be a variable"
          rootSymbol (Fun f _) = f


sig2ASig :: Bool -> ArgumentOptions -> SignatureSig s sDt -> ASignatureSig s sDt
sig2ASig isCf args (Signature (n,_,ctr,_) pre post) =
  Signature (n, newACost, ctr,isCf) (map newADatatype pre) (newADatatype post)
  where newADatatype (dt, csts) = ActualCost isCf dt (newACostVector args)

newACost :: ACost Vector
newACost = ACost (Vector1 0)

newACostVector      :: ArgumentOptions -> ACost Vector
newACostVector args = ACost 0


getTermVars :: Term f v -> [Term f v]
getTermVars (Var x)    = [Var x]
getTermVars (Fun _ ch) = concatMap getTermVars ch

termName           :: (Show f, Show v) => Term f v -> String
termName (Var n)   = show n
termName (Fun n _) = show n


--
-- InfRuleMisc.hs ends here
