-- | builds basic OBDDs

module OBDD.Make 

( constant, unit, variable, false, true )

where

import OBDD.Data

import Data.Map ( Map )
import qualified Data.Map as M

constant :: Ord v => Bool -> OBDD v
constant b = make $ do
    register $ Leaf b

false :: Ord v => OBDD v
false = constant False

true :: Ord v => OBDD v
true  = constant True

-- | Variable with given parity
unit :: Ord v => v -> Bool -> OBDD v
unit v p = make $ do
    l <- register $ Leaf $ not p
    r <- register $ Leaf $     p
    register $ Branch v l r

variable :: Ord v => v -> OBDD v
variable v = unit v True
