-- | reduced ordered binary decision diagrams
-- (c) Johannes Waldmann, 2008
--
-- this module is intended to be imported qualified
-- because it overloads some Prelude names.
--
-- for a similar, but much more elaborate project, see
-- <http://www.informatik.uni-kiel.de/~mh/lehre/diplomarbeiten/christiansen.pdf>
-- but I'm not sure where that source code would be available.

module OBDD 

( OBDD 
, module OBDD.Property
, module OBDD.Operation
, module OBDD.Make
) 

where

import OBDD.Data ( OBDD )
import OBDD.Property
import OBDD.Operation
import OBDD.Make
