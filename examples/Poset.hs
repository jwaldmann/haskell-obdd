{-# language LambdaCase #-}

import Prelude hiding ((&&),(||),not,and,or,Num)
import OBDD 

import Control.Monad ( guard )
import System.Environment ( getArgs )
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Data.Array as A
import Data.Ix
import Data.List (transpose, sortOn, partition)
import Data.IORef
import Control.Monad ( forM_, when)
import System.IO
import Data.Ratio

-- * driver

main = getArgs >>= \ case
  [] -> investigate 4
  [s] -> investigate $ read s

verbose = False

investigate n = do
  state <- newIORef 1
  let (c, pos) = posets n
  putStrLn $ unwords [ "number of interesting posets is", show c ]
  forM_ pos $ \ cs0 -> do
    let cs =  filter (\(x,y) -> x < y) cs0
    when verbose $ print cs
    best <- readIORef state
    let value (c,z) = 1 - max z (t-z) % t
        (t, es) = extensions n cs
        (far,near) = partition (\ this -> value this < best) es
    if (Prelude.null near) then do
      let f = last $ sortOn value far
          g = value f
      putStrLn ""
      print (fromRational g :: Double, g, cs, t, f)
      when (g < 1%3) $ error "huh"
      writeIORef state g
    else do
      putStr "."
      return ()
    hFlush stdout

-- * enumerate compatible permutations

extensions n cs = 
  let ps = map (\x -> map (\y -> (x,y)) [1..n] )[1..n]
      vars = S.fromList $ concat ps
      xss = map (map variable) ps
      ns = map number xss
      f = and $ ispermutation xss : map (compat ns) cs
      total = number_of_models vars f
  in  ( total
      , do
        x <- [1..n] ; y <-[x+1 .. n] ; guard $ x < y ; let c = (x,y)
        guard $ not $ elem c cs
        return (c, number_of_models vars $ compat ns c && f )
      )

vars n = S.fromList $ (,) <$> [1..n] <*> [1..n]

compatible n cs =
  let ps = map (\x -> map (\y -> (x,y)) [1..n] )[1..n]
      xss = map (map variable) ps
      ns = map number xss
  in  and $ ispermutation xss : map (compat ns) cs

compat ns (x,y) = leq (ns !! pred x) (ns !! pred y)

ispermutation :: Boolean b => [[b]] -> b
ispermutation xss =
     ( and $ map exactlyone xss )
  && ( and $ map exactlyone $ transpose xss )

exactlyone :: Boolean b =>  [b] -> b
exactlyone xs =
  let go (n,o) [] = o
      go (n,o) (x:xs) = go (choose n false x, choose o n x) xs
  in  go (true,false) xs

type Num b = [b]

number :: Boolean b => [b] -> Num b
number (x:xs) = scanl (||) x xs

lt :: Boolean b  => Num b -> Num b -> b
lt xs ys = or $ zipWith (\x y -> not x && y) xs ys

leq :: Boolean b  => Num b -> Num b -> b
leq xs ys = and $ zipWith ( ==> ) xs ys


-- * enumerate posets

posets n =
  let r = relation ((1,1),(n,n)) $ \ (i,j) ->
        case compare i j of
	  LT -> variable (i,j)
	  EQ -> true
	  GT -> false
      vars = S.fromList $ filter (\(i,j) -> i < j ) $ pairs r
      f = and [ poset r, connected r, upward r, notwins r, nocutvertex r ]
  in  ( number_of_models vars f
      , map edges $ models vars f
      )

nocutvertex r = and $ do
  x <- domain r
  return $ not $ cutvertex r x

cutvertex r x = and $ do
  y <- domain r
  return $ r ! (x,y) || r ! (y,x)

connected r = full $ transitive_closure $ r || mirror r

transitive_closure r = 
  iterate (\ r -> r || times r r) r !! log2 (length $ domain r)

log2 n = if n > 1 then succ $ log2 $ div n 2 else 0

poset r = reflexive r && antisymmetric r && transitive r

upward r = and $ do
  x <- source r ; y <- source r ; guard $ x > y
  return $ not $ r ! (x,y)

edges m = M.keys $ M.filter id m

notwins r = and $ do
  x <- source r ; y <- source r ; guard $ x < y
  return $ not $ twin r x y

twin r x y = and $ not (r ! (x,y)) : not (r!(y,x)) : do
  z <- domain r
  guard $ z /= x
  guard $ z /= y
  return $  equiv (r ! (x,z)) (r ! (y,z))
         && equiv (r ! (z,x)) (r ! (z,y))

-- * Relations

newtype Relation s t b = Relation (A.Array (s,t) b)

Relation a ! p = a A.! p

relation bnd f = Relation $ A.array bnd
  $ map (\i -> (i, f i)) $ A.range bnd

source :: Ix s => Relation s t b -> [s]
source (Relation a) =
  let ((al,at),(ar,ab)) = A.bounds a
  in  A.range (al,ar)

target :: Ix t => Relation s t b -> [t]
target (Relation a) =
  let ((al,at),(ar,ab)) = A.bounds a
  in  A.range (at,ab)

domain :: (Ix s, Eq s) => Relation s s b -> [s]
domain r | source r == target r  = source r

pairs (Relation a) = A.indices a

instance (Ix s, Ix t, Boolean b)
         => Boolean (Relation s t b) where
  not = map1 not
  (||) = map2 (||)
  (&&) = map2 (&&)
  (==>) = map2 (==>)
  xor = map2 xor
  bool f = error "bool"
  all = error "all"
  any = error "any"

full (Relation a) = and $ A.elems a
null r = and $ not r

map1 f (Relation a) =
  Relation $ A.listArray (A.bounds a)
             $ map f (A.elems a)
    
map2 f (Relation a) (Relation b) | A.bounds a == A.bounds b =
  Relation $ A.listArray (A.bounds a)
           $ zipWith f (A.elems a) (A.elems b) 

subsetOf a b = full $ a ==> b

transitive r = subsetOf (times r r) r

mirror (Relation a) = Relation $
  let ((al,at),(ar,ab)) = A.bounds a
      bnd = ((at,al),(ab,ar))
  in  A.array bnd $ do
        ((i,j),v) <- A.assocs a ; return ((j,i),v)

times (Relation a) (Relation b) = Relation $
   let ((al,at),(ar,ab)) = A.bounds a
       ((bl,bt),(br,bb)) = A.bounds b
       bnd = ((al,bt),(ar,bb))
   in  A.array bnd $ do
        (i,k) <- A.range bnd
        return ((i,k), or $ do
                   j <- A.range (at,ab)
                   return $ a A.!(i,j) && a A.!(j,k) )

reflexive (Relation a) =
  let ((al,at),(ar,ab)) = A.bounds a
  in  and $ do i <- A.range (al,ar) ; return $ a A.! (i,i)

diagonal (Relation a) = and $ do
       ((i,j),k) <- A.assocs a
       guard $ i /= j
       return $ not k

antisymmetric r = diagonal (r && mirror r)


