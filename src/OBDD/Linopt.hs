module OBDD.Linopt where

import OBDD (OBDD, fold)

import qualified Data.Map.Strict as M

type Item v w = (w, [(v,Bool)])

-- | solve the constrained linear optimisation problem:
-- returns an assignment that is a model of the BDD
-- and maximises the sum of weights of variables.
linopt :: ( Ord v , Num w, Ord w ) 
       => OBDD v 
       -> M.Map v w 
       -> Maybe (w, M.Map v Bool)
linopt d m = ( \(w,kvs) -> (w,M.fromList kvs) ) <$>
   fold ( \ leaf  -> if leaf then Just (0, []) else Nothing )
       ( \ v ml mr -> case (ml,mr) of
          (Nothing, Just r) -> Just $   add m v $ fill m v r
          (Just l, Nothing) -> Just $ noadd m v $ fill m v l
          (Just l,  Just r) -> Just $
                  let l' = noadd m v $ fill m v l 
                      r' =   add m v $ fill m v r
                  in  if fst l' >= fst r' then l' else r'
       )
       d

fill :: (Ord v, Num w) => M.Map v w -> v -> Item v w -> Item v w
fill m v (w, xs) = 
    let vs = (case xs of
               [] -> id
               (u,_):_ -> takeWhile (\(k,v) -> k > u) ) 
           $ dropWhile (\(k,_) -> k >= v) 
           $ M.toDescList m
    in  foldr (add m) (w, xs) $ map fst vs

noadd, add :: (Ord v, Num w) => M.Map v w -> v -> Item v w -> Item v w
noadd m v (w,xs) = (w          , (v,False) : xs)
add   m v (w,xs) = (w + m M.! v, (v, True) : xs)

