{-

The minimal number of knights needed to occupy or attack
every square on an n×n chessboard
(i.e., domination numbers for the n×n knight graphs)

http://mathworld.wolfram.com/KnightsProblem.html

This illustrates how to solve a Boolean linear optimisation problem
by bottom-up processing of BDD nodes using @fold@.
Note that we have to be careful because of variables
that are missing on paths.

-}

import Prelude hiding ((&&),(||),not,and,or)
import OBDD 

import Control.Monad ( guard )
import System.Environment ( getArgs )
import Data.List (sort)

type Position = (Int,Int)

positions :: Int -> [ Position ]
positions n = (,) <$> [1..n] <*> [1..n]

knight (a,b) (c,d) = 5 == (a-c)^2 + (b-d)^2

board :: Int -> OBDD Position
board n = and $ do
  p <- positions n ; q <- positions n
  guard $ p < q ; guard $ knight p q
  return $ not (variable p) || not (variable q)

main = do
    args <- getArgs
    case map read args :: [Int] of
        [] -> mainf 8
        [arg] -> mainf arg

mainf n = do
    let d :: OBDD Position
        d = board n
        
        pos = reverse $ sort $ positions n
        fill :: Position
             -> (Maybe Position, [Position])
             -> (Maybe Position, [Position])        
        fill v (l,a) = 
          let vs = (case l of
                       Nothing -> id
                       Just u  -> takeWhile (/= u) ) 
                 $ dropWhile (>= v) pos
          in  foldr add (l,a) vs
        add v (l,a) = (Just v, v : a)
              
        a :: Maybe (Maybe Position, [Position])
        a = fold ( \ f -> if f then Just (Nothing,[]) else Nothing )
            ( \ v l r -> case (l,r) of
                (Nothing, Just ar) -> Just $ add v $ fill v ar
                (Just al, Nothing) -> Just         $ fill v al
                (Just al, Just ar) -> Just $
                  let al' = fill v al ; ar' = add v $ fill v ar
                  in  if length (snd al') > length (snd ar')
                      then al' else ar'
            )
            d
    putStrLn $ unwords [ "board size", show n ]
    putStrLn $ unwords [ "BDD size", show $ OBDD.size d ]
    putStrLn $ show a
