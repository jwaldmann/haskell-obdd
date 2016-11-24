-- |  http://www2.stetson.edu/~efriedma/mathmagic/0916.html
-- On an N×N chessboard, when we place Q queens, 
-- what is the maximum number of squares 
-- that can be attacked exactly A times?

{-# language LambdaCase #-}

import Prelude hiding ((&&),(||),not,and,or)
import qualified Prelude as P
import OBDD 
import OBDD.Linopt

import Data.Ix (inRange)
import qualified Data.Array as A
import Control.Monad ( guard )
import System.Environment ( getArgs )
import Data.List (sort)
import qualified Data.Map.Strict as M

main = getArgs >>= \ case
  [] -> run 6 2 0
  [n,q,a] -> run (read n) (read q) (read a)  

run n q a = putStrLn $ form n q a
        $ linopt ( board n q a ) 
        $ M.fromList 
        $ zip ((\ p -> Var p Attacked) <$> positions n) (repeat 1)
        ++ zip ((\ p -> Var p Queen) <$> positions n) (repeat 0)

type Position = (Int,Int)

positions :: Int -> [ Position ]
positions n = (,) <$> [1..n] <*> [1..n]

data Type =  Attacked | Queen deriving (Eq, Ord, Show)
data Var = Var !Position !Type deriving (Eq, Ord, Show)

type Bit = OBDD Var

queen p = variable $ Var p Queen
attacked p = variable $ Var p Attacked


header n q a w = unwords 
   [ "n =", show n
   , "q =", show q
   , "a =", show a
   , "m =", show w 
   ] 

form n q a (Just (w,m)) = unlines $ header n q a w  : do
  row <- [1..n]
  return $ do
    col <- [1..n]
    let c = if m M.! Var (row,col) Queen then 'Q' 
            else if m M.! Var (row,col) Attacked then '+'
            else '.'
    [ c, ' ' ]

for = flip map

board :: Int -> Int -> Int -> Bit
board n q a = let r = ray n  in and 
    $ (  exactly q $ queen <$> positions n )
    : ( for ( positions n) $ \ p -> 
      (not $ queen p) || (not $ attacked p) )
    ++ ( for (positions n) $ \ p ->  attacked p ==>
           ( exactly a $ for directions $ \ d -> r A.! (d,p)))
    

-- | ray n ! (d,p) == looking in direction d from p,
--  there is (at least one) queen (which might be on p)
ray n = 
    let bounds = (((-1,-1),(1,1)),((1,1),(n,n)))
        result =  A.array bounds $ do
          (d,p) <- A.range bounds
          let q = shift d p
          return ( (d,p)
              , queen p
                  || if onboard n q then result A.! (d,q) else false
              )
    in  result
  

directions = filter (/= (0,0)) 
   $ (,) <$> [ -1 .. 1 ] <*> [ -1 .. 1 ]

onboard n (x,y) = inRange (1,n) x P.&& inRange (1,n) y
shift (dx,dy) (x,y) = (x+dx,y+dy)

exactly :: Int -> [Bit] -> Bit
exactly k xs = 
  if k <= 8 P.&& length xs <= 8 
  then exactly_direct k xs
  else exactly_rectangle k xs

exactly_rectangle n xs = last $ 
  foldl ( \ cs x -> zipWith ( \ a b -> choose a b x )
                    cs (false : cs) 
        ) (true : replicate n false) xs

exactly_direct k xs = atmost k xs && atleast k xs

atmost k xs = not $ atleast (k+1) xs
atleast k xs = or $ for (select k xs) and

select 0 xs = return []
select k [] = []
select k (x:xs) = select k xs ++ ( (x:) <$> select (k-1) xs )
