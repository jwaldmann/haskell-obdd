{-

The maximal number of non-attacking knights on an n×n chessboard
(i.e., independence numbers for the n×n knight graphs)

http://mathworld.wolfram.com/KnightsProblem.html

-}

import Prelude hiding ((&&),(||),not,and,or)
import OBDD 
import OBDD.Linopt

import Control.Monad ( guard )
import System.Environment ( getArgs )
import Data.List (sort)
import qualified Data.Map.Strict as M

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
        [] -> mainf 9
        [arg] -> mainf arg

mainf n = do
    let d :: OBDD Position
        d = board n
        a = linopt d $ M.fromList $ zip (positions n) $ repeat 1
    putStrLn $ unwords [ "board size", show n ]
    putStrLn $ unwords [ "BDD size", show $ OBDD.size d ]
    putStrLn $ show a
