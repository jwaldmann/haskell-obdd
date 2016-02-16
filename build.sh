#!/bin/bash

# call with one argument which is the ghc version, e.g. 7.8.4

VER=$1
echo building for VER=$VER
export PATH=/opt/ghc/ghc-$VER/bin:$PATH

cabal sandbox delete
cabal sandbox init
time cabal install
