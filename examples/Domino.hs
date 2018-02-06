{-
In how many ways can a n x m board be entirely covered by 2 x 1 domino
pieces?

Usage: ./Domino 4 3
   or: ghci> prettySolve 4 3
-}
module Main where

import Control.Monad (guard)
import System.Environment (getArgs)
import qualified Data.Map
import qualified Data.Set

import qualified OBDD

dominoes w h = rows ++ cols
    where rows = positions [1..h-1] [1..w] $ \x y -> ((x, y), (x+1, y))
          cols = positions [1..h] [1..w-1] $ \x y -> ((x, y), (x, y+1))
          positions xs ys f = do
            x <- xs
            y <- ys
            return (f x y)

positions w h = do
    x <- [1..h]
    y <- [1..w]
    return (x, y)

dominoFormula w h = OBDD.and [ everyPosCovered, noPosCoveredTwice ]
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
          fields = positions w h
          placements = dominoes w h
          f `isCoveredBy` p = f == fst p || f == snd p

prettySolution w h ds = putStr $ concat $ do
    x <- [1..h]
    y <- [1..w]
    let letter = case find (\(domino, _) -> fst domino == (x, y) || snd domino == (x, y)) withNames of
                     Nothing -> "."
                     Just (_, letter) -> letter:[]
    return $ if y == w then letter ++ "\n" else letter
    where withNames = if length names < length ds
                      then error "not enough names"
                      else zip ds names
          names = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
          find f xs = case filter f xs of
                          []  -> Nothing
                          x:_ -> Just x

prettySolve w h =
    mapM_ (\ds -> prettySolution w h ds >> putStrLn "")
    . map (map fst . filter snd)
    . map (Data.Map.toList) $
    OBDD.models (Data.Set.fromList $ dominoes w h) $ dominoFormula w h

main = do
    args <- getArgs
    let [width, height] = case map read args :: [Int] of
              [] -> [4, 3]
              [width, height] -> [width, height]
    let formula = dominoFormula width height
    mapM_ (\ds -> prettySolution width height ds >> putStrLn "")
        . map (map fst . filter snd)
        . map (Data.Map.toList) $
        OBDD.models (Data.Set.fromList $ dominoes width height) formula
    print $ OBDD.number_of_models (Data.Set.fromList $ dominoes width height) formula
