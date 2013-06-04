{-
the N Queens problem. The propositional variables
correspond to the positions on the board.
It shows how to construct an OBDD
and how to check some of its properties.
It also shows that the implementation is not terribly efficient.
It computes the number of solutions for board size 8
(the answer is: 92) in approx. 1.6 seconds on my machine.

BUILD:  ghc -O2 Queens
RUN  :  ./Queens 8
-}

import OBDD (OBDD)
import qualified OBDD
import qualified OBDD.Data (toDot)

import Control.Monad ( guard )
import System.Environment ( getArgs )
import qualified Data.Set 

type Position = (Int,Int)

positions :: Int -> [ Position ]
positions n = do 
    a <- [ 1 .. n ]
    b <- [ 1 .. n ]
    return (a,b)

threatens :: Position -> Position -> Bool
threatens (a,b) (c,d) = 
       a == c     -- same column
    || b == d     -- same row
    || a+b == c+d -- same diagonal
    || a-b == c-d -- same antidiagonal

board :: Int -> OBDD Position
board n = OBDD.and 
    [ each_column_is_occupied n
    , no_threats n
    ]

each_column_is_occupied n = OBDD.and $ do
    a <- [ 1 .. n ]
    return $ OBDD.or $ do
        b <- [ 1 .. n ]
        return $ OBDD.unit (a,b) True

no_threats n = OBDD.and $ do
    p <- positions n
    return $ OBDD.and $ do
        q <- positions n
        guard $ p < q
        guard $ threatens p q
        return $ OBDD.or [ OBDD.unit p False, OBDD.unit q False ]

main = do
    [ arg ] <- getArgs
    let n :: Int ; n = read arg

    let d :: OBDD Position
        d = board n

    print $ OBDD.size d
    print $ OBDD.satisfiable d

    print $ OBDD.number_of_models 
          ( Data.Set.fromList $ positions n )
          d

    m <- OBDD.some_model d
    print m

    -- writeFile "Queens.dot" $ OBDD.Data.toDot d