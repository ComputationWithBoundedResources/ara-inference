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
--     Update #: 476
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
import           Data.Rewriting.ARA.ByInferenceRules.Prove
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


mkCompletelyDefinedConds :: (Ord f, Read v, Eq f, Eq v, Show f, Show dt, Eq dt, Show v) =>
                            Prove f v f dt dt f
                         -> Prove f v f dt dt f
mkCompletelyDefinedConds prove =
  -- trace ("grRls: " ++ show grRls)
  -- trace ("defSyms: " ++ show defSyms)
  -- trace ("allSyms: " ++ show allSyms)
  -- trace ("ctrSyms: " ++ show ctrSyms)
  -- trace ("lhss: " ++ show lhssChlds) $
  -- trace ("ctrArities: " ++ show ctrArities)
  -- trace ("nRules: " ++ show nRules)
  -- trace ("ctrArities: " ++ show ctrArities)
  -- undefined
  -- trace ("nSigs: " ++ show (addSig <$> signatures p))
  -- undefined
  -- p { rules = (rules p) { strictRules = strictRules (rules p) ++ concat nRules }
  --   , datatypes = addDt <$> datatypes p
  --   , signatures = addSig <$> signatures p
  --   }
  -- undefined

  -- import qualified Control.Exception                                         as E
  -- import           Data.Rewriting.ARA.Exception
  -- E.throw $ if null (concat nRules) then FatalException "YES" else FatalException "NO"

  prove { conditions = nCond, signatureMap = nSigM }


  where -- addDt = (++ [Datatype (read (show rhsBtmSym))
        --               [ Constructor (read $ show rhsBtmSym) []]])
        -- addSig = (++ [Signature (read (show rhsBtmSym)) [] (read (show rhsBtmSym))])
        p = problem prove
        (nCond, nSigM) = foldl mkSigCond (conditions prove, signatureMap prove) (concat nRules)
        mkSigCond acc@(cond,sigs) (Rule (Fun _ lhss) (Fun f _))
          | not (null varParamNrs) = (cond',sigs')
          | otherwise = acc

            -- foldl (mkSigCond' f) acc (zip [0..] lhss)

          where (nr,sigs') =
                  case find ((== f) . fst4 . lhsRootSym . fst3 . snd) (zip [0..] sigs) of
                    Nothing     -> -- trace ("nsig: " ++ show nSig)
                                   (length sigs, sigs ++ nSig)
                    Just (nr,_) -> (nr, sigs)
                sig = getDefSymSignatureByName' (fromJust $ signatures p) f
                nCondDt pNr = -- trace ("nCond p(" ++ show nr ++ "," ++ show pNr ++") == 0")
                  (SigRefParam "" nr pNr, Eq, 0)
                nSig = [(Signature (f,ACost (Vector1 0),False, False)
                        (map (\dt -> ActualCost False dt (ACost 0)) pDts)
                        (ActualCost False rDt (ACost 0))
                        ,-1, "to make completely defined")]
                pDts = map fst $ lhsSig sig
                rDt = fst $ rhsSig sig

                cond' = cond { dtConditionsInt = dtConditionsInt cond
                               ++ map nCondDt varParamNrs }
                varParamNrs :: [Int]
                varParamNrs = map fst $ filter (hasVars . return . snd) (zip [0..] lhss)

        -- mkSigCond' f (cond,sigs) (pNr,lhs) = case lhs of
        --   Var _    ->
        --     trace ("nCond: " ++ show ((nr, pNr::Int) , Eq, 0))
        --     ( cond { dtConditionsInt = dtConditionsInt cond ++ nCond}
        --     , sigs'
        --     )
        --   Fun f' [] -> case arity f of
        --     Just chLen -> foldl (mkSigCond' f') (cond,sigs)
        --       (zip [0..] (replicate chLen (Var undefined)))
        --   Fun f' ch -> foldl (mkSigCond' f') (cond,sigs) (zip [0..] ch)

        --   where (nr,sigs') =
        --           case find ((== f) . fst4 . lhsRootSym . fst3 . snd) (zip [0..] sigs) of
        --             Nothing     -> (length sigs, sigs' ++ nSig)
        --             Just (nr,_) -> (nr, sigs)
        --         sig = getDefSymSignatureByName' (fromJust $ signatures p) f
        --         nCond = [(SigRefParam "" (nr::Int) (pNr::Int) , Eq, 0)]
        --         nSig = [(Signature (f,ACost (Vector1 0),False, False)
        --                 (map (\dt -> ActualCost False dt (ACost 0)) pDts)
        --                 (ActualCost False rDt (ACost 0))
        --                , -1, "to make completely defined")]
        --         pDts = map fst $ lhsSig sig
        --         rDt = fst $ rhsSig sig

        isVar Var{} = True
        isVar _     = False
        hasVars xs = any isVar xs || any hasVars chlds
          where chlds = map chld xs
                chld (Fun _ ch) = ch
                chld _          = []


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
              maxDepths = map (maximum . map maxDepthF) (transpose lhss)
              arityFuns = -- Var "x" :
                concat (zipWith zipAritiesToFun ctrArities ctrSyms)

              paramLen = length (head lhss)
              ctrCombs0 =  combsTerm arityFuns (length $ filter not paramVars)
                (map snd $ filter (not.fst) (zip paramVars maxDepths))
              ctrParams0 = map (map dropAritiesFromFun) ctrCombs0
              ctrParams = map (\ps -> fst $
                foldl (\(acc,combs) (var,varsOnly) ->
                          if varsOnly
                          then (acc++mkDummyVar varsOnly var,combs)
                          else (acc++[head combs],tail combs)) ([],ps) (zip [1..] paramVars))
                          ctrParams0
              filteredParams = filter hasVars $ foldl filterParams ctrParams lhss

              hasVarsBelRoot = any hasVars . map chld
                where chld (Fun _ ch) = ch
                      chld _          = []
              mkRule params = Rule (Fun f params) (Fun  f [])
              isVar Var{} = True
              isVar _     = False
              paramVars = map (all isVar) $ transpose lhss
              mkDummyVar True nr = [Var $ read $ show $ "x" ++ show nr]
              mkDummyVar False _ = [Fun f []]

          in -- trace ("\n\nf: " ++ show f) $
             -- -- trace ("lhss: " ++ show lhss) $
             -- trace ("arityFuns: " ++ show arityFuns) $

             -- -- trace ("res: " ++ show (length $ concat $
             -- -- mergeVars (length ctrSyms) filteredParams)) $
             -- trace ("length ctrCombs: " ++ show (length ctrCombs0))
             -- trace ("ctrCombs0: " ++ show ctrCombs0)
             -- trace ("paramVars: " ++ show paramVars)
             -- -- trace ("ctrParams0: " ++ show ctrParams0)
             -- trace ("ctrParams: " ++ show ctrParams)
             -- trace ("filteredParams: " ++ show (filteredParams)) $
             -- trace ("length filteredParams: " ++ show (length filteredParams)) $
             -- trace ("maxDepth: " ++ show maxDepth)
             -- trace ("merged: " ++ show (mergeVars (length ctrSyms) paramVars filteredParams))
             -- trace ("merged: " ++ show (map mkRule $ mergeVars (length ctrSyms) paramVars
             --                            filteredParams)) $

             -- trace ("res: " ++ show (
             --           zipWith3 (\varsOnly var ps ->
             --                       if varsOnly then mkDummyVar varsOnly var
             --                       else trace ("ps: " ++ show ps) ps) paramVars [1..]
             --           (mergeVars (length ctrSyms) paramVars filteredParams))) $

             if and paramVars
             then []            -- only variables (no need for constraints)
             else -- map mkRule (zipWith mkDummyVar paramVars [1..]) ++
                  map mkRule (mergeVars (length ctrSyms) paramVars filteredParams)

mergeVars :: (Show f, Show v, Read v, Eq v, Eq f) => Int -> [Bool] -> [[Term f v]] -> [[Term f v]]
mergeVars _ _ [] = -- trace ("empty mergeVars []" )
  []
mergeVars nrCtrs paramVars xs@(x:_) =
  nub $ map (zipWith4 mergeVars' paramVars [0..] canBeMerged) xs
  where mergeVars' True nr _ _ = Var (read $ show $ "x" ++ show nr)
        mergeVars' _ nr True _ = Var (read $ show $ "x" ++ show nr)
        mergeVars' _ _ False x = x
        canBeMerged = map allConstrs (zip [0..] x)
        allConstrs (idx,p)
          | -- trace ("length filt: " ++ show (length filt))
            -- trace ("nrCtrs: " ++ show nrCtrs)

            length filt == nrCtrs = True
          | otherwise = False
          where filt = filter (eqButSelf x idx) xs
                eqButSelf ps idx ps' = case splitAt idx (zipWith (==) ps ps') of
                  (xs, [_]) -> and xs
                  (xs, ys)  -> and (xs ++ tail ys)


filterParams :: (Show f, Show v, Eq f) => [[Term f v]] -> [Term f v] -> [[Term f v]]
filterParams [] rule     = []
filterParams pCombs rule = filter (rule `notIncludes`) pCombs

notIncludes :: (Eq f) => [Term f v] -> [Term f v] -> Bool
notIncludes pRules pCombs = or (zipWith notIncludes' pRules pCombs)
  where notIncludes' Var{} _                 = False
        notIncludes' (Fun f ch) (Var _)      = True
        notIncludes' (Fun f ch) (Fun f' ch') = f /= f' || notIncludes ch ch'


dropAritiesFromFun :: (Read v) => Term (Int, f) String -> Term f v
dropAritiesFromFun (Fun (_,f) ch) = Fun f (map dropAritiesFromFun ch)
dropAritiesFromFun Var{}          = Var (read $ show "x")

zipAritiesToFun :: Int -> f -> [Term (Int,f) String]
-- zipAritiesToFun arity f | arity == 0 = []
zipAritiesToFun arity f = return $ Fun (arity,f) (replicate arity (Var "x"))


combineParamChoices :: (Show v, Show f) => [[Term (Int, f) v]] -> [[Term (Int, f) v]]
combineParamChoices = combineParamChoices' []
  where combineParamChoices' :: (Show v, Show f) =>
          [[Term (Int, f) v]] -> [[Term (Int, f) v]] -> [[Term (Int, f) v]]
        combineParamChoices' [] (x:xs) = combineParamChoices' (map return x) xs
        combineParamChoices' acc (x:xs) =
            combineParamChoices' (concat [ [ pL ++ [x'] | pL <- acc ] | x' <- x ]) xs
        combineParamChoices' acc [] = acc


addChldsCtr :: (Show v, Show f) => [Term (Int, f) v] -> Term (Int, f) v -> [Term (Int, f) v]
addChldsCtr ctrs p@Var{} = [p]
addChldsCtr ctrs p@(Fun (0,_) _)= [p]
addChldsCtr ctrs p@(Fun (nr,f) _) =
  -- trace ("\n\npossCombs: " ++ show possCombs)
  -- trace ("outChldCtr: " ++ show (map (\x -> Fun (nr,f) x) possCombs))
  map (Fun (nr,f)) possCombs
  where possCombs = combs ctrs nr
-- addChldsCtr ctrs p@(Fun f ch) =
--   trace ("p: " ++ show p)
--   trace ("combs: " ++ show (map (addChldsCtr ctrs) ch))
--   trace ("combs': " ++ show chlds')
--   trace ("out: " ++ show (map (Fun f) chlds')) $
--   map (Fun f) chlds'
--   where chlds' = combineParamChoices (map (addChldsCtr ctrs) ch)


combsTerm :: (Show v, Show f) => [Term (Int,f) v] -> Int -> [Int] -> [[Term (Int,f) v]]
combsTerm  ctrs argLen maxDepths = filter (not.toDeep) (combsTerm'  ctrs argLen (maximum maxDepths))
  where toDeep xs = or (zipWith isTooDeep maxDepths xs)
        isTooDeep maxD term = depth term > maxD
        depth (Fun f ch) = 1 + maximum (0:map depth ch)
        depth Var{}      = 0

combsTerm' :: (Show v, Show f) => [Term (Int,f) v] -> Int -> Int -> [[Term (Int,f) v]]
combsTerm' ctrs argLen maxDepth | argLen < 1 = []
combsTerm' ctrs argLen maxDepth | maxDepth < 1 = []
combsTerm' ctrs argLen maxDepth = combs' [] 0
  where base = combs ctrs argLen
        combs'  _  0  = combs' base 1
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


-- --
-- -- CompletelyDefined.hs ends here
