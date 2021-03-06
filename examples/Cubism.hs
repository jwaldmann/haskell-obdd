{-# LANGUAGE TupleSections #-}
{-# language LambdaCase #-}

import Prelude hiding ((&&),(||),not,and,or)
import OBDD
import OBDD.Cube
import System.Environment (getArgs)

data T = In | L | R | Out   deriving (Eq, Ord, Show)

vars n = (,) <$> [1..n] <*> [L,R,Out]

main = getArgs >>= \ case
 []  -> out $ cnf $ form add 3
 [ "add", s] -> out $ cnf $ form add $ read s
 [ "mul", s] -> out $ cnf $ form mul $ read s
 [ "hist", s] -> out $ cnf $ function (read s) hist
 [ "sort", s] -> out $ cnf $ function (read s) sort
 "king" : cs -> out $ cnf $ constrained_function 9 $ king (map read cs)

out cs = mapM_ (\(k,v) -> putStrLn $ show k ++ " " ++ nice v)
       $ zip [0..] cs

function w f = 
  let input = map ( variable . (, In) ) [1 .. w]
      output = map ( variable . (, Out) ) [1 .. ]
  in  and $ zipWith equiv (f input) output

constrained_function w f =
  let input = map ( variable . (, In) ) [1 .. w]
      output = map ( variable . (, Out) ) [1 .. ]
      (c,out) = f input
  in  and $ c : zipWith equiv out output

-- | for MM 12/16
king hs (this : neighbours) =
  let ys = hist neighbours
      cs = map (\ h -> this && ys !! h) hs
  in  ( this ==> or cs , cs )

sort xs =
  let insert ys x =
        zipWith ( \ a b -> a || b && x ) ys ( true : ys )
  in  foldl insert (replicate (length xs) false) xs

-- | histogram xs == ys where  ys !! i <=> exactly i xs,
-- produces a one-hot bit vector.
-- length output = 1 + length input
hist xs =
  let insert ys x =
        zipWith ( \ a b -> choose a b x ) ys ( false : ys )
  in  foldl insert (true : replicate (length xs) false) xs
  
form fun n =
  let make f = ( \ v -> variable (v,f)) <$> [1..n]
      (pre,post) = splitAt n $ fun (make L) (make R)
  in  not (or post) && and ( zipWith equiv pre $ make Out )

mul [] ys = []
mul (x:xs) ys = add (map (&& x) ys) $ false : mul xs ys

add xs ys =
  let go c [] [] = [c]
      go c (x:xs) [] = let (r,d) = halfadd c x in r : go d xs []
      go c [] (y:ys) = let (r,d) = halfadd c y in r : go d ys []
      go c (x:xs) (y:ys) = let (r,d) = fulladd c x y in r : go d xs ys
  in  go false xs ys
      
halfadd x y = (xor x y , x && y)

fulladd x y z =
  let (r , c)  = halfadd x y
      (s , d) = halfadd r z
  in  (s, c || d)

