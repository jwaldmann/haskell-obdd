{-# language LambdaCase #-}

import Prelude hiding ((&&),(||),not,and,or,Num)
import qualified Prelude
import qualified Data.Bool 
import OBDD hiding (size)
import qualified OBDD as O

import Control.Monad ( guard, forM_, when, void, mzero, msum )
import System.Environment ( getArgs )
import System.IO (hFlush, stdout)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (sort, sortOn, tails, transpose)
import qualified Data.Tree as T
import Data.Maybe (isJust)

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

-- * poset enumeration

data State =
     State { comps:: ! [Comp]
           , poset :: ! Poset
           , args :: ! [Num]
           , form :: ! Bit
           , size :: ! Integer
           }

start w =
  let i = input w
      f = ispermutation i
  in State { comps = []
           , poset = mkposet []
           , args = map number i
           , form = f
           , size = number_of_models (vars w) f
           }

next :: Int -> State -> Comp -> State
next w s c =
  let cs' = c : comps s
      f = compat (args s) c && form s
  in  s { comps = cs'
        , poset = -- mkposet cs'
	    transitive_closure $ S.insert c $ poset s
        , form = f
        , size = number_of_models (vars w) f
        }

run w d = do
  putStrLn $ unwords [ "sort", show w, "items", "with", show d, "comparisons" ]
  (r, cache) <- work w d (start w) M.empty
  putStrLn ""
  putStrLn $ unwords [ "sort", show w, "items", "with", show d, "comparisons", "is"
     , Data.Bool.bool "IMPOSSIBLE" "POSSIBLE" r ]
  putStrLn $ unwords [ "cache", "with", show (M.size cache), "entries" ]
  when False $ forM_ (M.toList cache) $ \(k,v) -> do
    putStrLn $ unwords [ show k, "=>", show v ]
    -- forM_ (M.toList m) print
  return r

work w d s known = do
  -- print (d,comps s,size s)
  if size s == 1
    then return (True,known)
    else if size s > 2^d
         then return (False,known)
         else do
	   let verbose = False
           case M.lookup (canonical $ poset s) known of
             Just (r,prev) -> do
               if verbose
	         then putStrLn $ unwords [ show d, show $ size s, show (comps s)
                                  , show r, "iso", show prev ]
		 else putStr "!"
               return (r,known)
             Nothing -> do
               let go [] known = return (False, known)
                   go (c@(x,y):cs) known = do
		     let [s1,s2] = reverse
		                 $ sortOn size
		                 $ map (next w s) [ (x,y), (y,x) ]
                     (a1,k1) <- work w (d-1) s1 known
                     if a1
                       then do
                         (a2,k2) <- work w (d-1) s2 k1
                         if a2
                           then return (True, k2)
                           else go cs k2
                       else do
                         go cs k1
               let candidates =
                     filter (\ (x,y) -> Prelude.not $ S.member (x,y) $ poset s)
                     $ filter (\ (x,y) -> Prelude.not $ S.member (y,x) $ poset s)
                      $ comparators w
               (r,known) <- go candidates known
               if verbose
	         then putStrLn $ unwords [ show d, show $ size s, show (comps s)
                                  , show r ]
                 else putStr "." 
  	       hFlush stdout	 
               return
                 (r, M.insert (canonical $ poset s) (r, comps s) known)

-- * main

main = getArgs >>= \ case
  [ ] -> void $ run 4 5
  [ w ] -> let b = ceiling
                 $ logBase 2 $ fromIntegral
                 $ factorial $ read w
           in -- search (read w) b
	       void $ run (read w) b
  [ w , d ] -> void $ run (read w) (read d)


search w d = run w d >>= \ case
  True -> return ()
  False -> search w (d+1)
  
factorial n = product [1 .. n]

-- * posets and their isomorphisms

type Poset = S.Set Comp

mkposet comps = transitive_closure $ S.fromList comps

dot :: Poset -> Poset -> Poset
dot p q = S.fromList $ do
  (x,y1) <- S.toList p
  (y2,z) <- S.toList q
  guard $ y1 == y2
  return (x,z)

transitive_closure :: Poset -> Poset
transitive_closure p =
  let q = S.union p $ dot p p
  in  if p == q then p else transitive_closure q
      
inputs  p x = map fst $ filter ((== x) . snd) $ S.toList p
outputs p x = map snd $ filter ((== x) . fst) $ S.toList p

elements p = S.union ( S.map fst p ) (S.map snd p )

data Type = Dot | Type [Type] [Type] deriving (Eq, Ord, Show)

types :: Poset -> M.Map Int Type
types p = M.fromList $ zip (S.toList $ elements p) $ repeat Dot

refine :: Poset ->   M.Map Int Type -> M.Map Int Type
refine p t = M.fromList $ do
  x <- S.toList $ elements p
  return (x, Type ( sort $ map (t M.!) $ inputs  p x )
                  ( sort $ map (t M.!) $ outputs p x ) )

classes :: M.Map Int Type -> M.Map Type (S.Set Int)
classes m = M.fromListWith S.union $ do
  (k,v) <- M.toList m
  return (v, S.singleton k)

-- | compare with keys
essence t = M.toAscList $ M.map S.size $ classes t

canonical po =
  let go t p =
        let t' = refine po t
            c' = classes t
            p' = sort $ M.elems c'
        in  if p == p' then M.map S.size c' else go t' p'
  in  go (types po) []


               
