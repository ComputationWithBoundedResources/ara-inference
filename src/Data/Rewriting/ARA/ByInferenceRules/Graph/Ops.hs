-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Aug 21 18:35:27 2016 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 34
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

module Data.Rewriting.ARA.ByInferenceRules.Graph.Ops
    ( analyzeReachability
    ) where

import           Data.Graph
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule

import           Control.Arrow
import           Data.Array
import           Data.Function
import           Data.List
import           Data.Maybe

import           Debug.Trace

analyzeReachability :: (Eq f, Ord f) =>
                       Problem f v s sDt dt cn
                    -> [(f,Integer)]
analyzeReachability prob =
  concat $ zipWith (\a b -> (map (\x -> (fst3 $ fromVert (fst x),b)) a))
  (reverse reachGroups) [0..]

  where ruls = allRules (rules prob)
        fs = map (tail . filter (`elem` defFuns) . funs) ruls
        getTopFun (Fun f _) = [f]
        getTopFun _         = []
        defFuns = concatMap (getTopFun . lhs) ruls
        edgs =
          map (\((a,b,c):xs) -> (a,b,c++concatMap thd3 xs) ) $
          groupBy ((==) `on` fst3) $
          sortBy (compare `on` fst3) $
          zipWith (\a b -> (a,a,b)) defFuns fs
        (graph,fromVert,toVertM) = graphFromEdges edgs
        edgeConns = edges graph
        toVert = fromJust . toVertM
        reaches = map (toVert . fst3 &&& reachable graph . toVert . fst3) edgs
        reachSorts = sortBy (\a b -> if fst a `elem` snd b
                                    then GT
                                    else if fst  b `elem` snd a then LT else
                                           compare (length $ snd b) (length $ snd a))
                     reaches
        reachGroups = groupBy (\a b -> fst a `elem` snd b && fst b `elem` snd a
                              ) reachSorts


--
-- Ops.hs ends here
