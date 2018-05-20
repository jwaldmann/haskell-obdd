{-
In how many ways can a n x m board be entirely covered by 2 x 1 domino
pieces?

Usage: ./Domino 4 3
       time stack runghc examples/Domino.hs 4 9
       VERBOSE=1 stack runghc examples/Domino.hs 2 3
   or: ghci> prettySolve 4 3
-}
module Main where

import Prelude hiding (bool,not,and,or,any,all,(&&),(||))
import Control.Monad (guard, when)
import Data.Maybe (isJust)
import System.Environment (getArgs, lookupEnv)
import qualified Data.Map
import qualified Data.Set
import Data.List (tails)

import OBDD

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

exactly_one xs = or xs && atmost_one xs
atmost_one xs = and $ do
  x : ys <- tails xs ; y <- ys ; return $ not $ x && y

dominoFormula w h =
  all ( \ f -> exactly_one (map variable $ filter (isCoveredBy f) (dominoes w h) )
      ) (positions w h)

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
    models (Data.Set.fromList $ dominoes w h) $ dominoFormula w h

main = do
    args <- getArgs
    let [width, height] = case map read args :: [Int] of
              [] -> [4, 3]
              [width, height] -> [width, height]
    let formula = dominoFormula width height
    verbose <- lookupEnv "VERBOSE" >>= return . isJust
    when verbose $ mapM_ (\ds -> prettySolution width height ds >> putStrLn "")
                   . map (map fst . filter snd)
                   . map (Data.Map.toList) $
                   models (Data.Set.fromList $ dominoes width height) formula
    print $ number_of_models (Data.Set.fromList $ dominoes width height) formula
