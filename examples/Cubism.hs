{-# language LambdaCase #-}

import Prelude hiding ((&&),(||),not,and,or)
import OBDD
import OBDD.Cube
import System.Environment (getArgs)

data T = L | R | Out   deriving (Eq, Ord, Show)

vars n = (,) <$> [1..n] <*> [L,R,Out]

main = getArgs >>= \ case
 [ "add", s] -> out $ cnf $ form add $ read s
 [ "mul", s] -> out $ cnf $ form mul $ read s

out cs = mapM_ (\(k,v) -> putStrLn $ show k ++ " " ++ nice v)
       $ zip [0..] cs

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

