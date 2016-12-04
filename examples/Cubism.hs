{-# LANGUAGE TupleSections #-}
{-# language LambdaCase #-}

import Prelude hiding ((&&),(||),not,and,or)
import OBDD
import OBDD.Cube
import System.Environment (getArgs)

data T = In | L | R | Out   deriving (Eq, Ord, Show)

vars n = (,) <$> [1..n] <*> [L,R,Out]

main = getArgs >>= \ case
 [ "add", s] -> out $ cnf $ form add $ read s
 [ "mul", s] -> out $ cnf $ form mul $ read s
 [ "hist", s] -> out $ cnf $ function (read s) histogram 
 [ "sort", s] -> out $ cnf $ function (read s) sortnet

out cs = mapM_ (\(k,v) -> putStrLn $ show k ++ " " ++ nice v)
       $ zip [0..] cs

function w f = 
  let input = map ( variable . (, In) ) [1 .. w]
      output = map ( variable . (, Out) ) [1 .. ]
  in  and $ zipWith equiv (f input) output

sortnet xs =
  let insert ys x =
        zipWith ( \ a b -> a || b && x ) ys ( true : ys )
  in  foldl insert (replicate (length xs) false) xs

-- | histogram xs == ys where  ys !! i <=> exactly i xs,
-- produces a one-hot bit vector.
histogram xs =
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

