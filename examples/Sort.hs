{-# language LambdaCase #-}

import Prelude hiding ((&&),(||),not,and,or,Num)
import qualified Prelude
import OBDD hiding (size)
import qualified OBDD as O

import Control.Monad ( guard, forM_, when, void, mzero, msum )
import System.Environment ( getArgs )
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (sortOn, tails, transpose)
import qualified Data.Tree as T

import Debug.Trace

-- | we will talk about permutation matrices,
-- so we need to index their elements.
type Bit = OBDD (Int,Int)

ispermutation :: [[Bit]] -> Bit
ispermutation xss =
     ( and $ map exactlyone xss )
  && ( and $ map exactlyone $ transpose xss )

exactlyone :: [Bit] -> Bit
exactlyone xs =
  let go (n,o) [] = o
      go (n,o) (x:xs) = go (bool n false x, bool o n x) xs
  in  go (true,false) xs

-- | (weakly) increasing sequence of bits
type Num = [Bit] 

-- | produce a number from a sequence that has exactly one bit set.
number :: [Bit] -> [Bit]
number (x:xs) = scanl (||) x xs

lt :: Num -> Num -> Bit
lt xs ys = or $ zipWith (\x y -> not x && y) xs ys

leq :: Num -> Num -> Bit
leq xs ys = and $ zipWith implies xs ys

type Comp = (Int,Int)

comparators :: Int -> [Comp]
comparators w =
  [0 .. w-2] >>= \ x -> [x+1..w-1] >>= \ y -> [(x,y)]

compat :: [Num] -> Comp -> Bit
compat ns (lo,hi) = leq (ns !! lo) (ns !! hi)

input w = do
  i <- [1..w]
  return $ map (\j -> variable (i,j))[1..w]

vars w = S.fromList $ (,) <$> [1..w] <*> [1..w]

-- | a sorting (branching) program is either done
-- and it knows the proper order of elements,
-- or it compares to elements, and branches according to the result.
data Sorter = Done [Int] | Ask (Integer, Comp) Sorter Sorter

instance Show Sorter where
  show s =
    let go s = case s of
          Done xs -> T.Node (show xs) []
          Ask c l r -> T.Node (show c) [go l, go r]
    in T.drawTree $ go s

data State =
     State { comps:: ! [Comp]
           , args :: ! [Num]
           , form :: ! Bit
           , size :: ! Integer
           }

start w =
  let i = input w
      f = ispermutation i
  in State { comps = []
           , args = map number i
           , form = f
           , size = number_of_models (vars w) f
           }

improved_start w =
   foldl (next w) (start w) $ map (\i -> (2*i,2*i+1)) [ 0 .. div w 2 - 1 ]

next :: Int -> State -> Comp -> State
next w s c =
  let cs' = c : comps s
      f = compat (args s) c && form s
  in  s { comps = cs'
        , form = f
        , size = number_of_models (vars w) f
        }

-- | (lazy) list of sorters for w elements, with depth at most d
sorters w d =
  let go d s = -- trace (show (d, size s)) $
        if 1 == size s then return $ Done $ decode w $ form s
        else if 2 ^ d < size s then mzero
             else msum {- $ take 1 -} $ do
               (c, s1 , s2) <-
                   sortOn (\(c, s1,s2) -> max(size s1)(size s2)) $
                 do
                   comp@(x,y) <- comparators w
                   let s1 = next w s (x,y)
                   guard $ 1 <= size s1 Prelude.&& size s1 <= 2^(d-1)
                   let s2 = next w s (y,x)
                   guard $ 1 <= size s2 Prelude.&& size s2 <= 2^(d-1)
                   return (comp, s1,s2)
               return $ if size s1 > size s2
                 -- taking the larger side first
                 -- (in the hope it fails fast)
                 then do
                    l <- go (d-1) s1 ; r <- go (d-1) s2
                    return $ Ask (size s, c) l r
                 else do
                    r <- go (d-1) s2 ; l <- go (d-1) s1 
                    return $ Ask (size s, c) l r
      s = improved_start w
  in  go (d - length (comps s)) s

decode w f = do
  let [ m ] = all_models f
  i <- [ 1 .. w ]
  return $ length $ dropWhile Prelude.not $ do
    j <- [ 1 .. w ]
    return $ m M.! (i,j)
    
main = getArgs >>= \ case
  [ ] -> void $ run 4 5
  [ w ] -> let b = ceiling
                 $ logBase 2 $ fromIntegral
                 $ factorial $ read w
           in search (read w) b
  [ w , d ] -> void $ run (read w) (read d)

run w d = do
  putStrLn $ unwords [ "find a sort algorithm for", show w,
                       "inputs with at most", show d, "comparisons" ]
  putStrLn $ unwords [ "initial comparisons", show $ comps $ improved_start w ]
  case sorters w d of
    Nothing    -> return False
    Just s -> do print s ; return True

search w d = run w d >>= \ case
  True -> return ()
  False -> search w (d+1)
  
factorial n = product [1 .. n]
