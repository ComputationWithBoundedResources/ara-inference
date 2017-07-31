-- CompletelyDefined.hs ---
--
-- Filename: CompletelyDefined.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Jul 31 14:31:27 2017 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 40
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

module Data.Rewriting.ARA.CompletelyDefined
    ( mkCompletelyDefined
    ) where

import           Data.Rewriting.Typed.Datatype
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature


import           Control.Lens
import           Control.Monad.State
import           Data.Function                  (on)
import           Data.List
import qualified Data.Map.Strict                as M
import qualified Data.Text                      as T
import           Text.PrettyPrint.ANSI.Leijen

import           Debug.Trace


mkCompletelyDefined :: (Ord f, Eq f, Show f, Show v) =>
                       Problem f v s sDt dt f
                    -> Problem f v f String String f
mkCompletelyDefined p =
  trace ("grRls: " ++ show grRls)
  trace ("defSyms: " ++ show defSyms)
  trace ("allSyms: " ++ show allSyms)
  trace ("ctrSyms: " ++ show ctrSyms)
  trace ("lhss: " ++ show lhssChlds) $
  trace ("allPossLhss: " ++ show allPossLhss)
  trace ("nRules: " ++ show nRules)
  undefined


  where rls = allRules (rules p)
        rootTerm (Rule (Fun f _) _) = f
        rootTerm (Rule (Var v) _)   = error "not possible"

        grRls = groupBy ((==) `on` rootTerm) $
                sortBy (compare `on` rootTerm) rls
        defSyms = map (rootTerm.head) grRls

        funSyms (Rule lhs rhs) = funSymsTerm lhs ++ funSymsTerm rhs
        funSymsTerm (Fun f ch) = f : concatMap funSymsTerm ch
        funSymsTerm (Var _)    = []
        allSyms = nub $ concatMap funSyms rls
        ctrSyms = filter (`notElem` defSyms) allSyms

        defTerms t@(Fun f ch) = (f, t) : concatMap defTerms ch
        defTerms t@(Var v)    = []

        allTerms = concatMap (\(Rule lhs rhs) -> defTerms lhs ++ defTerms rhs) rls
        arity f = case find ((== f).fst) allTerms of
          Nothing           -> Nothing -- must be a variable
          Just (_,Fun _ ch) -> Just $ length ch
          Just _            -> Nothing -- is a variable


        rootTermChlds (Rule (Fun _ ch) _) = ch
        rootTermChlds (Rule _ _)          = error "not possible"
        lhssChlds = map (map rootTermChlds) grRls

        allPossLhss = map arity ctrSyms
        nRules = map mkCompletelyDefined' lhssChlds
        mkCompletelyDefined' :: [[Term f v]] -> [Rule f v]
        mkCompletelyDefined' lhss =
          let maxDepthF (Var x)    = 0 :: Int
              maxDepthF (Fun _ ch) = (1 :: Int) + maximum (0:map maxDepthF ch)
              maxDepth = maximum $ concatMap (map maxDepthF) lhss
          in trace ("maxDepth: " ++ show maxDepth)
             undefined


-- [Var "x",Fun "0"  [],Fun "s" [Var "x"],Fun "s" [Var "y"]]


--
-- CompletelyDefined.hs ends here
