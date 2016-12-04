{-# language FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module OBDD.Cube where

import Prelude hiding ((&&),(||),not,and,or)
import qualified Prelude
import OBDD

import qualified Data.Bool 
import Control.Monad (guard)
import Control.Applicative

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (partition, sortOn)

import Debug.Trace

type Cube v = M.Map v Bool

primes :: Ord v => OBDD v -> [Cube v]
primes d =
  let process m = M.fromList $ do
        ((v,Sign), b) <- M.toList m
        return (v, b)
  in map process $ paths $ prime d

nice :: Show v => Cube v -> String
nice c = "(" ++ do
    (v,b) <- M.toList c
    (if b then " " else " -") ++ show v
  ++ ")"

data Check = Sign | Occurs | Original
           deriving (Eq, Ord, Show)

sign v = variable (v, Sign)
occurs v = variable (v, Occurs)
original v = variable (v, Original)

process c = M.fromList $ do
        ((v,Occurs),True) <- M.toList c
        return (v, c M.! (v,Sign))


-- | O. Coudert , J. C. Madre:
-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.3330

prime :: Ord v => OBDD v -> OBDD (v, Check)
prime = snd
  . fold ( \ b -> ( bool b, bool b ))
         ( \ v (fl,pl) (fr,pr) -> ( choose fl fr (variable v)
               , let common = prime (fl && fr)
                 in      not (occurs v) && common
                     || occurs v && not (sign v) && pl && not common
                     || occurs v &&     (sign v) && pr && not common
               )
         )


-- * naive way of finding a minimal set of clauses.

dnf f =
  let ps = primes f
      ms = models (variables f) f
      covering = do
        m <- ms
        return $ S.fromList $ do
                   (i,p) <- zip [0..] ps
                   guard $ M.isSubmapOf p m
                   return i
      cost = M.fromList $ zip [0..] $ map M.size ps
      r = greed cost covering
  in  map (ps !!) $ S.toList r


cnf f = map (M.map Prelude.not) $ dnf $ not f

greed cost [] = S.empty
greed cost could =
  let count = M.unionsWith (+) $ do
        c <- could
        return $ M.fromList $ zip (S.toList c) $ repeat 1
      this = fst
           $ head
           $ sortOn snd
           $ map (\(k,v) -> (k, (negate v, cost M.! k)))
           $ M.toList count 
  in    S.insert this
      $ greed cost $ filter ( \p -> S.notMember this p ) could

clause c = and $ do (v,b) <- M.toList c ; return $ unit v b

