module OBDD.Linopt (linopt) where

import OBDD (OBDD, full_fold)

import qualified Data.Map.Strict as M
import Data.Bool (bool)

-- | solve the constrained linear optimisation problem:
-- returns an assignment that is a model of the BDD
-- and maximises the sum of weights of variables.
-- Keys missing from the weight map, but present in the BDD,
-- get weight zero.
linopt :: ( Ord v , Num w, Ord w ) 
       => OBDD v 
       -> M.Map v w 
       -> Maybe (w, M.Map v Bool)
linopt d m = full_fold (M.keysSet m) 
   ( bool Nothing ( Just (0, M.empty) ))
   ( \ v ml mr -> case (ml,mr) of
          (Just l, Nothing) -> Just $ noadd m v l
          (Nothing, Just r) -> Just $   add m v r
          (Just l,  Just r) -> Just $
                  let l' = noadd m v l
                      r' =   add m v r
                  in  if fst l' >= fst r' then l' else r'
          -- the following *can* happen for
          -- interpolated nodes directly above False:
          (Nothing, Nothing) -> Nothing
       )
       d

type Item v w = (w, M.Map v Bool)

noadd, add :: (Ord v, Num w) 
           => M.Map v w -> v -> Item v w -> Item v w
noadd m v (w, b) = 
  (w                          , M.insert v False b)
add   m v (w, b) = 
  (w + M.findWithDefault 0 v m, M.insert v True  b)
