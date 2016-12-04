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
display d = void
  $ readProcessWithExitCode "dot" [ "-Tx11" ]
  $ toDot False d

-- | same as @display@, but does not show the @False@ node
-- and the edges pointing to @False@.
display' :: (Ord v, Show v) => OBDD v -> IO ()
display' d = void
  $ readProcessWithExitCode "dot" [ "-Tx11" ]
  $ toDot True d

-- | a textual representation of the BDD that is suitable
-- as input to the "dot" program from the graphviz suite.
toDot :: (Ord v, Show v)
  => Bool -- ^ suppress pointers to False
  -> OBDD v -> Text
toDot suppress_false =
    snd
  . ( \ m -> evalRWS (do tell "digraph BDD {"; m; tell "}" ) () 0 )
  . foldM
  ( \ b -> if not b && suppress_false then return Nothing else do
     this <- fresh
     tell $ T.unlines
       [ "node[shape=rectangle];"
       , text this <> mkLabel (text b)
       , "node[shape=oval];"
       ]
     return $ Just this
  )
  ( \ v ml mr -> do
     this <- fresh
     tell $ text this <> mkLabel (text v)
     case ml of
       Just l -> tell $ text this <> "->" <> text l <> mkLabel "0"
       Nothing -> return ()
     case mr of
       Just r -> tell $ text this <> "->" <> text r <> mkLabel "1"
       Nothing -> return ()
     return $ Just this
  )

fresh :: Monoid b => RWS a b Int Int
fresh = do this <- get ; put $! succ this ; return this

mkLabel lbl = "[label=\"" <> unquote lbl <> "\"];"

unquote s =
  if T.isPrefixOf "\"" s && T.isSuffixOf "\"" s
  then T.init $ T.tail s
  else s

text x = T.pack $ show x
