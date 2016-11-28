{-# language OverloadedStrings #-}

module OBDD.Display where

import OBDD.Data (OBDD, foldM)
import Control.Monad (void)
import Control.Monad.RWS (RWS, evalRWS, tell, get, put)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import System.Process.Text

-- | Calls the @dot@ executable (must be in @$PATH@)
-- to draw a diagram in an X11 window.
-- Will block until this window is closed.
-- Window can be closed gracefully
-- by typing  'q'  when it has focus.
display :: (Ord v, Show v) => OBDD v -> IO ()
display d =
  void $ readProcessWithExitCode "dot" [ "-Tx11" ] $ toDot d

-- | a textual representation of the BDD that is suitable
-- as input to the "dot" program from the graphviz suite.
toDot :: (Ord v, Show v) => OBDD v -> Text
toDot = snd . ( \ m -> evalRWS (do tell "digraph BDD {"; m; tell "}" ) () 0 ) . foldM
  ( \ b -> do
     this <- fresh
     tell $ T.unlines
       [ "node[shape=rectangle];"
       , text this <> mkLabel (text b)
       , "node[shape=oval];"
       ]
     return this
  )
  ( \ v l r -> do
     this <- fresh
     tell $ T.unlines
       [ text this <> mkLabel (text v)
       , text this <> "->" <> text l <> mkLabel "0"
       , text this <> "->" <> text r <> mkLabel "1"
       ]
     return this
  )

fresh :: Monoid b => RWS a b Int Int
fresh = do this <- get ; put $! succ this ; return this

mkLabel lbl = "[label=\"" <> unquote lbl <> "\"];"

unquote s =
  if T.isPrefixOf "\"" s && T.isSuffixOf "\"" s
  then T.init $ T.tail s
  else s

text x = T.pack $ show x
