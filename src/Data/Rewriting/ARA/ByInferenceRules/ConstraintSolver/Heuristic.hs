
module Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.Heuristic where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type


data Heuristic dt = Shift dt   -- shift
  | Diamond dt                  -- first element
  | Interleaving Bool dt dt     -- interleaving (bool for reverse)
  | Zero                        -- representing 0
  | One dt
  deriving (Show)


instance Functor Heuristic where
  fmap f (Shift dt)               = Shift (f dt)
  fmap f (Diamond dt)             = Diamond (f dt)
  fmap f (Interleaving b dt1 dt2)=Interleaving b (f dt1) (f dt2)
  fmap _ Zero                     = Zero
  fmap f (One dt)                 = One (f dt)

