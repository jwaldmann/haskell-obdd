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
      ms = -- paths f
        models (variables f) f
      covering = do
        m <- ms
        return $ S.fromList $ do
                   (i,p) <- zip [0..] ps
                   guard $ M.isSubmapOf p m
                   return i
      r = greed covering
  in  map (ps !!) $ S.toList r


cnf f = map (M.map Prelude.not) $ dnf $ not f

greed [] = S.empty
greed could =
  let count = M.unionsWith (+) $ do
        c <- could
        return $ M.fromList $ zip (S.toList c) $ repeat 1
      this = fst $ last $ sortOn snd $ M.toList count 
  in    S.insert this
      $ greed $ filter ( \p -> S.notMember this p ) could
        

reduce [] = S.empty
reduce covering = 
  let (singular,plural) = partition ((==1) . S.size) covering
  in if Prelude.null singular then greed plural
     else 
      let pick = S.unions singular
          remain = filter ( S.null . S.intersection pick ) plural
      in  S.union pick $ reduce remain


clause c = and $ do (v,b) <- M.toList c ; return $ unit v b

