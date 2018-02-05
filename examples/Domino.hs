{-
In how many ways can a n x m board be entirely covered by 2 x 1 domino
pieces?
-}
module Domino where

import Control.Monad (guard)
import System.Environment (getArgs)
import qualified Data.Set

import qualified OBDD

dominoes h w = rows ++ cols
    where rows = positions [1..h-1] [1..w] $ \x y -> ((x, y), (x+1, y))
          cols = positions [1..h] [1..w-1] $ \x y -> ((x, y), (x, y+1))
          positions xs ys f = do
            x <- xs
            y <- ys
            return (f x y)

positions h w = do
    x <- [1..h]
    y <- [1..w]
    return (x, y)

dominoFormula h w = OBDD.and [ everyPosCovered, noPosCoveredTwice ]
    where everyPosCovered = OBDD.and $ do
                                -- forall fields f \in F
                                f <- fields
                                return $ OBDD.or $ do
                                -- forall placements of pieces p \in P
                                    p <- placements
                                    return $ OBDD.and [
                                        OBDD.unit p True,                 -- placement is chosen
                                        OBDD.constant (f `isCoveredBy` p) -- f is covered by placement p
                                     ]
          noPosCoveredTwice = OBDD.and $ do
                                  f <- fields
                                  return $ OBDD.not $ OBDD.or $ do
                                      p <- placements
                                      return $ OBDD.or $ do
                                          q <- placements
                                          guard $ p /= q
                                          return $ OBDD.and [
                                              OBDD.unit p True,
                                              OBDD.constant (f `isCoveredBy` p),

                                              OBDD.unit q True,
                                              OBDD.constant (f `isCoveredBy` q)
                                           ]
          fields = positions h w
          placements = dominoes h w
          f `isCoveredBy` p = f == fst p || f == snd p

printPos h w pos = putStr $ concat $ do
    x <- [1..h]
    y <- [1..w]
    let l = if (x, y) == pos
            then "x"
            else "."
    return $ if y == w
             then l ++ "\n"
             else l

printPositions h w ps = putStr $ concat $ do
    x <- [1..h]
    y <- [1..w]
    let l = if any (== (x, y)) ps
            then "x"
            else "."
    return $ if y == w
             then l ++ "\n"
             else l

printDominoes h w ds = printPositions h w $ concat $ map (\(p, q) -> [p, q]) ds

main = do
    args <- getArgs
    let [width, height] = case map read args :: [Int] of
              [] -> [3, 4]
              [width, height] -> [width, height]
    print $ OBDD.number_of_models (Data.Set.fromList $ dominoes height width)
          $ dominoFormula height width
