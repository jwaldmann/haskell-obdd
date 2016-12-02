-- | This should really use theory from O. Coudert , J. C. Madre:
-- A New Method to Compute Prime and Essential Prime Implicants of Boolean Functions (1993) 
-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.461.7854

{-# language FlexibleContexts #-}

module OBDD.Cube where

import Prelude hiding ((&&),(||),not,and,or)
import OBDD

import qualified Data.Bool 
import Control.Monad (guard)
import Control.Applicative

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | A cube is a minimal conjunction of literals.
-- It represents a set of models (paths to True in the BDD).
-- "Minimal" means that if any literal is dropped from a cube,
-- then it represents at least one path that is not a model.
type Cube v = M.Map v Bool

data Check = Sign | Occur deriving (Eq, Ord, Show)


lift :: Ord v => OBDD v -> OBDD (v, Check)
lift = fold  bool
  ( \ v l r ->  not (variable (v,Sign)) && l
             || variable (v,Sign) && r
  )

relprime :: Ord v => [v] -> OBDD v -> OBDD (v, Check)
relprime vars f =
  let ff = lift f
      correct = foldr exists ff $ map (\v -> (v,Occur)) vars
      
  in  correct
