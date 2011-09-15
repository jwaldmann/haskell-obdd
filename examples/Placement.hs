{-
Auf wie viele Weisen kann man Spielsteine auf ein 3 x 10-Spielbrett setzen, 
sodass keine zwei Steine horizontal, vertikal oder diagonal benachbart sind?

(Quelle: Preisaufgabe bei LSGM-Wochenendseminar 2011 in Bennewitz
http://www.lsgm.de/tiki-index.php?page=Seminare.2011-09)

BUILD:  ghc --make Placement
RUN  :  ./Placement 3 10
-}

import OBDD (OBDD)
import qualified OBDD

import Control.Monad ( guard, forM_ )
import System.Environment ( getArgs )
import qualified Data.Set 

type Position = (Int,Int)

positions :: Int -> Int -> [ Position ]
positions width height = do 
    a <- [ 1 .. width  ]
    b <- [ 1 .. height ]
    return (a,b)

adjacent :: Position -> Position -> Bool
adjacent (a,b) (c,d) = 
    abs (a-c) <= 1 && abs (b-d) <= 1
  
main = do
    args <- getArgs
    let [ width, height ] = map read args :: [ Int ]
        ps = positions width height
    print $ OBDD.number_of_models ( Data.Set.fromList ps )
          $ OBDD.and $ do
           p <- ps
           q <- ps
           guard $ p < q 
           guard $ adjacent p q
           return $ OBDD.or [ OBDD.unit p False, OBDD.unit q False ]




