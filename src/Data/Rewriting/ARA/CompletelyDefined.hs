{-# LANGUAGE ScopedTypeVariables #-}
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
--     Update #: 232
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
    ( mkCompletelyDefinedConds
    ) where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature.Type
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.Operator.Type
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.Typed.Datatype
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature

import           Control.Lens
import           Control.Monad.State
import           Data.Function                                              (on)
import           Data.List
import qualified Data.Map.Strict                                            as M
import           Data.Maybe
import qualified Data.Text                                                  as T
import           Text.PrettyPrint.ANSI.Leijen                               hiding ((<$>))

import           Debug.Trace


mkCompletelyDefinedConds :: (Ord f, Eq f, Show f, Show dt, Eq dt, Show v) =>
                            ProblemSig f v f dt dt f
                         -> (ACondition f v Int Int, ASigs dt f)
mkCompletelyDefinedConds p =
  -- trace ("grRls: " ++ show grRls)
  -- trace ("defSyms: " ++ show defSyms)
  -- trace ("allSyms: " ++ show allSyms)
  -- trace ("ctrSyms: " ++ show ctrSyms)
  -- trace ("lhss: " ++ show lhssChlds) $
  -- trace ("ctrArities: " ++ show ctrArities)
  trace ("nRules: " ++ show nRules)

  -- trace ("nSigs: " ++ show (addSig <$> signatures p))
  -- undefined
  -- p { rules = (rules p) { strictRules = strictRules (rules p) ++ concat nRules }
  --   , datatypes = addDt <$> datatypes p
  --   , signatures = addSig <$> signatures p
  --   }
  -- undefined

  ret


  where -- addDt = (++ [Datatype (read (show rhsBtmSym))
        --               [ Constructor (read $ show rhsBtmSym) []]])
        -- addSig = (++ [Signature (read (show rhsBtmSym)) [] (read (show rhsBtmSym))])
        ret = foldl mkSigCond (ACondition [] [] [] [] [], []) nRules
        mkSigCond acc [] =  acc
        mkSigCond (cond,sigs) (Rule (Fun _ lhss) (Fun f _):_) =
          ( cond { dtConditionsInt = dtConditionsInt cond ++
                    map (\p -> (SigRefParam "" (nr::Int) (p::Int) , Eq, 0))
                    [0..(length lhss-1)]
                 }

           , sigs ++ [(Signature (f,ACost (Vector1 0),False, False)
                       (map (\dt -> ActualCost False dt (ACost 0)) pDts)
                       (ActualCost False rDt (ACost 0))
                      , -1, "to make completely defined")]
           )
          where nr = length sigs
                sig = getDefSymSignatureByName' (fromJust $ signatures p) f
                pDts = map fst $ lhsSig sig
                rDt = fst $ rhsSig sig

        rls = allRules (rules p)
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

        ctrArities = catMaybes $ map arity ctrSyms
        nRules = map mkCompletelyDefined' (zip defSyms lhssChlds)
        -- mkCompletelyDefined' :: [[Term f v]] -> [Rule f v]
        mkCompletelyDefined' (f,lhss) =
          let maxDepthF (Var x)    = 0
              maxDepthF (Fun _ ch) = 1 + maximum (0:map maxDepthF ch)
              maxDepth = maximum $ concatMap (map maxDepthF) lhss
              arityFuns = zipWith zipAritiesToFun ctrArities ctrSyms
              paramLen = length (head lhss)
              ctrCombs = combsTerm arityFuns paramLen maxDepth
              ctrParams = map (map dropAritiesFromFun) ctrCombs
              filteredParams = foldl (flip filterParams) ctrParams lhss
              mkRule params = Rule (Fun f params) (Fun  f [])
          in map mkRule filteredParams

filterParams :: (Eq f) => [Term f v] -> [[Term f v]] -> [[Term f v]]
filterParams rule []     = []
filterParams rule pCombs = filter (rule `notIncludes`) pCombs

notIncludes :: (Eq f) => [Term f v] -> [Term f v] -> Bool
notIncludes pRules pCombs = or (zipWith notIncludes' pRules pCombs)
  where notIncludes' (Var{}) _               = False
        notIncludes' (Fun f ch) (Var _)      = True
        notIncludes' (Fun f ch) (Fun f' ch') = f /= f' || notIncludes ch ch'


dropAritiesFromFun :: Term (Int, f) String -> Term f v
dropAritiesFromFun (Fun (_,f) ch) = Fun f (map dropAritiesFromFun ch)

zipAritiesToFun :: Int -> f -> Term (Int,f) String
zipAritiesToFun arity f = Fun (arity,f) []


combineParamChoices :: (Show v, Show f) => [[Term (Int, f) v]] -> [[Term (Int, f) v]]
combineParamChoices xs =
  -- trace ("\n xs In: " ++ show xs) $
  -- trace ("xs out: " ++ show (  combineParamChoices' [] xs))
  combineParamChoices' [] xs
  where combineParamChoices' :: (Show v, Show f) =>
          [[Term (Int, f) v]] -> [[Term (Int, f) v]] -> [[Term (Int, f) v]]
        combineParamChoices' [] (x:xs) = combineParamChoices' (map return x) xs
        combineParamChoices' acc (x:xs) =
            combineParamChoices' (concat [ [ pL ++ [x'] | pL <- acc ] | x' <- x ]) xs
        combineParamChoices' acc [] = acc


addChldsCtr :: (Show v, Show f) => [Term (Int, f) v] -> Term (Int, f) v -> [Term (Int, f) v]
addChldsCtr ctrs p@(Fun (0,_) _)= [p]
addChldsCtr ctrs p@(Fun (nr,f) []) =
  -- trace ("\n\npossCombs: " ++ show possCombs)
  -- trace ("outChldCtr: " ++ show (map (\x -> Fun (nr,f) x) possCombs))
  map (\x -> Fun (nr,f) x) possCombs
  where possCombs = combs ctrs nr
addChldsCtr ctrs p@(Fun f ch) =
  -- trace ("combs: " ++ show (map (addChldsCtr ctrs) ch))
  -- trace ("combs': " ++ show chlds')
  -- trace ("out: " ++ show (map (Fun f) chlds')) $
  map (Fun f) chlds'
  where chlds' = combineParamChoices (map (addChldsCtr ctrs) ch)


combsTerm :: (Show v, Show f) => [Term (Int,f) v] -> Int -> Int -> [[Term (Int,f) v]]
combsTerm ctrs argLen maxDepth | argLen < 1 = []
combsTerm ctrs argLen maxDepth | maxDepth < 1 = []
combsTerm ctrs argLen maxDepth = combs' [] 0
  where base = combs ctrs argLen
        combs'  _  0  = combs' [ b | b <- base ] 1
        combs' acc nr
          | maxDepth == nr = acc
          | otherwise = combs' (concat [ combineParamChoices
                                         [ addChldsCtr ctrs p | p <- sig ]
                                       | sig <- acc ]) (nr+1)


combs :: [a] -> Int ->  [[a]]
combs base upTo | upTo < 1 = []
combs base upTo = combs' base [] 0
  where combs' base _   0  = combs' base [ [b] | b <- base] 1
        combs' base acc nr
          | upTo == nr = acc
          | otherwise = combs' base (concat [ map (b:) acc | b <- base]) (nr+1)


-- [Var "x",Fun "0"  [],Fun "s" [Var "x"],Fun "s" [Var "y"]]


--
-- CompletelyDefined.hs ends here
