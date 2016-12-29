A pure Haskell implementation of Reduced Orderded Binary Decision Diagrams.
---------------------

[![Build Status](https://travis-ci.org/jwaldmann/haskell-obdd.svg)](http://travis-ci.org/jwaldmann/haskell-obdd)

This is mostly educational. The BDDs do not share nodes and this might introduce inefficiencies.

An important (for me, in teaching) feature is
that I can immediately draw the BDD to an X11 window (via graphviz).
For example, to show the effect of different variable orderings,
try this in ghci:
```
import qualified Prelude as P
import OBDD
let f [] = false; f (x:y:zs) = x && y || f zs
display P.$ f P.$ P.map variable [1,2,3,4,5,6]
display P.$ f P.$ P.map variable [1,4,2,5,3,6]
```

If you want better performance,
use [CUDD](http://vlsi.colorado.edu/%7Efabio/CUDD/)
Haskell bindings ([low level](https://hackage.haskell.org/package/cudd),
[high level](https://github.com/jwaldmann/cudd-ersatz)).
