{-
the N Queens problem (alternative implementation).
The propositional variables
correspond to the positions on the board.
It shows how to construct an OBDD
and how to check some of its properties.
It also shows that the implementation is not terribly efficient.
It computes the number of solutions for board size 8
(the answer is: 92) in approx. 1.6 seconds on my machine.

BUILD:  ghc -O2 Queens
RUN  :  ./Queens 8
-}

import Prelude hiding ((&&),(||),not,and,or)
import OBDD 

import Control.Monad ( guard )
import System.Environment ( getArgs )
import qualified Data.Set 
import qualified Data.Map.Strict as M

type Position = (Int,Int)

positions :: Int -> [ Position ]
positions n = do 
    a <- [ 1 .. n ]
    b <- [ 1 .. n ]
    return (a,b)

board :: Int -> OBDD Position
board n = and 
    [ handle exactlyone (\(x,y) -> x) n
    , handle atmostone  (\(x,y) -> y) n
    , handle atmostone  (\(x,y) -> x+y) n
    , handle atmostone  (\(x,y) -> x-y) n
    ]

atmostone xs =
  let go (n,o) [] = n || o
      go (n,o) (x:xs) = go (choose n false x, choose o n x) xs
  in  go (true,false) xs

exactlyone xs =
  let go (n,o) [] = o
      go (n,o) (x:xs) = go (choose n false x, choose o n x) xs
  in  go (true,false) xs

handle check f n = OBDD.and $ do
    (k,v) <- M.toList $ M.fromListWith (++)
          $ map (\p -> (f p, [variable p])) $ positions n
    return $ check v

main = do
    args <- getArgs
    case map read args :: [Int] of
        [] -> mainf 8
        [arg] -> mainf arg

mainf n = do
    let d :: OBDD Position
        d = board n
    putStrLn $ unwords [ "board size", show n ]
    putStrLn $ unwords [ "BDD size", show $ OBDD.size d ]
    putStrLn $ unwords [ "number of models"
                       , show $ OBDD.number_of_models 
                         ( Data.Set.fromList $ positions n )
                         d
                       ]
