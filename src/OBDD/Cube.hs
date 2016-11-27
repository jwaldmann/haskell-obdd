module OBDD.Cube where

import OBDD.Data ( fold, OBDD )

import Data.Bool (bool)
import Control.Monad (guard)
import Control.Applicative

import qualified Data.Map.Strict as M

-- | A cube is a minimal conjunction of literals.
-- It represents a set of models (paths to True in the BDD).
-- "Minimal" means that if any literal is dropped from a cube,
-- then it represents at least one path that is not a model.
type Cube v = M.Map v Bool

cubes :: Ord v => OBDD v -> [ Cube v ]
cubes = fold
  ( bool [] [ M.empty ] )
  ( \ v l r -> ( M.insert v False <$> l ) ++ (M.insert v True <$> r )
               ++ do cl <- l ; cr <- r ; intersection cl cr )

intersection :: (Ord v, Monad m, Alternative m) => Cube v -> Cube v -> m (Cube v)
intersection f g = do
  guard $ and $ M.intersectionWith (==) f g
  return $ M.union f g
