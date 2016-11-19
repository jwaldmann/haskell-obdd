-- | Reduced ordered binary decision diagrams,
-- pure Haskell implementation.
-- (c) Johannes Waldmann, 2008
--
-- This module is intended to be imported qualified
-- because it overloads some Prelude names.
--
-- For a similar, but much more elaborate project, see
-- <http://www.informatik.uni-kiel.de/~mh/lehre/diplomarbeiten/christiansen.pdf>
-- but I'm not sure where that source code would be available.

module OBDD 

( OBDD, display
, module OBDD.Property
, module OBDD.Operation
, module OBDD.Make
) 

where

import OBDD.Data ( OBDD, display )
import OBDD.Property
import OBDD.Operation
import OBDD.Make
