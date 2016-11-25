{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}
{-# language FlexibleContexts #-}
{-# language TupleSections #-}

-- | implementation of reduced ordered binary decision diagrams.

module OBDD.Data 

(
-- * the data type
 OBDD -- abstract
, size
-- * for external use
, null, satisfiable
, number_of_models
, paths, models
, fold, foldM
, full_fold, full_foldM
, toDot, display
-- * for internal use
, Node (..)
, make
, register, checked_register
, cached, top
, access

)

where

import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM

import OBDD.IntIntMap (IntIntMap)
import qualified OBDD.IntIntMap as IIM

import OBDD.VarIntIntMap (VarIntIntMap)
import qualified OBDD.VarIntIntMap as VIIM

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M

import qualified Data.Array as A

import Data.Set ( Set )
import qualified Data.Set as S
import Data.Bool (bool)

import Control.Arrow ( (&&&) )
import Control.Monad.State.Strict
   (State, runState, evalState, get, put, gets, modify)
import qualified System.Random
import Control.Monad.Fix
import Control.Monad ( forM, guard, void )
import qualified Control.Monad ( foldM )
import Data.Functor.Identity
import System.Process
import Data.List (isPrefixOf, isSuffixOf)

import Prelude hiding ( null )
import qualified Prelude

-- newtype Index = Index { unIndex :: Int }
--   deriving ( Eq, Ord, Num, Enum, Show )

type Index = Int ; unIndex = id

-- | assumes total ordering on variables
data OBDD v = OBDD
            { core :: !(IntMap ( Node v Index ))
            
            -- , icore :: !(Map ( Node v Index ) Index)
            , icore :: !(VarIntIntMap v Index)
            , next :: !Index
            , top :: !Index
            
            , cache ::  !(IntIntMap Index) 
               -- ^ inputs and output for binary op
               -- (unary will be simulated by binary)
            }


-- | Apply function in each node, bottom-up.
-- return the value in the root node.
-- Will cache intermediate results.
-- You might think that 
-- @count_models = fold (\b -> if b then 1 else 0) (\v l r -> l + r)@ 
-- but that's not true because a path might omit variables.
-- Use @full_fold@ to fold over interpolated nodes as well.
fold :: Ord v 
     => ( Bool -> a )
     -> ( v -> a -> a -> a )
     -> OBDD v -> a
fold leaf branch o = runIdentity 
   $ foldM ( return . leaf )
           ( \ v l r -> return $ branch v l r )
           o

-- | Run action in each node, bottum-up.
-- return the value in the root node.
-- Will cache intermediate results.
foldM :: (Monad m, Ord v)
     => ( Bool -> m a )
     -> ( v -> a -> a -> m a )
     -> OBDD v -> m a
foldM leaf branch o = do
    f <- leaf False ; t <- leaf True
    let m0 = M.fromList 
           [(icore_false,f), (icore_true,t)]
    m <- Control.Monad.foldM ( \ m (i,n) -> do
            val <- case n of
                Branch v l r -> 
                        branch v (m M.! l) (m M.! r) 
            return $ M.insert i val m
          ) m0 $ IM.toAscList $ core o
    return $ m M.! top o

-- | Apply function in each node, bottom-up.
-- Also apply to interpolated nodes: when a link
-- from a node to a child skips some variables:
-- for each skipped variable, we run the @branch@ function
-- on an interpolated node that contains this missing variable,
-- and identical children.
-- With this function, @number_of_models@
-- can be implemented as 
-- @full_fold vars (bool 0 1) ( const (+) )@.
-- And it actually is, see the source.
full_fold :: Ord v 
     => Set v
     -> ( Bool -> a )
     -> ( v -> a -> a -> a )
     -> OBDD v -> a
full_fold vars leaf branch o = runIdentity 
   $ full_foldM vars 
           ( return . leaf )
           ( \ v l r -> return $ branch v l r )
           o

full_foldM :: (Monad m, Ord v)
     => Set v 
     -> ( Bool -> m a )
     -> ( v -> a -> a -> m a )
     -> OBDD v -> m a
full_foldM vars leaf branch o = do
    let vs = S.toAscList vars
        low = head vs
        m = M.fromList $ zip vs $ tail vs
        up v = M.lookup v m
        interpolate now goal x | now == goal = return x
        interpolate (Just now) goal x = 
            branch now x x >>= interpolate (up now) goal
    (a,res) <- foldM 
          ( \ b -> (Just low ,) <$> leaf b )
          ( \ v (p,l) (q,r) -> do
                l' <- interpolate p (Just v) l
                r' <- interpolate q (Just v) r
                (up v,) <$> branch v l' r'
          ) o
    interpolate a Nothing res  

icore_false = 0 :: Index  
icore_true = 1 :: Index  
              
size = unIndex . next

-- | Number of satisfying assignments with  given set of variables.
-- The set of variables must be given since the current OBDD may not contain
-- all variables that were used to construct it, since some  nodes may have been removed
-- because they had identical children.
number_of_models :: Ord v => Set v -> OBDD v ->  Integer
number_of_models vars o = 
  full_fold vars (bool 0 1) ( const (+) ) o

empty :: OBDD v
empty = OBDD 
      { core = IM.empty
      , icore = VIIM.empty
      , next = 2
      , top = 0
      , cache = IIM.empty
      }

data Node v i = Leaf !Bool
            | Branch !v !i !i
    deriving ( Eq, Ord )

access :: OBDD v -> Node v ( OBDD v )
access s = case top s of
    0 -> Leaf False
    1 -> Leaf True
    t -> case IM.lookup ( top s ) ( core s ) of
        Nothing -> error "OBDD.Data.access"
        Just n  -> case n of
            Leaf p -> error "Leaf in core"
            Branch v l r -> 
                Branch v ( s { top = l } ) 
                         ( s { top = r } )

-- | does the OBDD have any models?
satisfiable :: OBDD v -> Bool
satisfiable = not . null

-- | does the OBDD not have any models?
null :: OBDD v -> Bool
null s = case access s of
      Leaf False -> True 
      _ -> False

-- | randomly select one model, if possible
some_model :: Ord v 
           => OBDD v 
           -> IO ( Maybe  ( Map v Bool ) )
some_model s = case access s of
    Leaf True -> return $ Just $ M.empty
    Leaf False -> return Nothing
    Branch v l r -> do
        let nonempty_children = do
                 ( p, t ) <- [ (False, l), (True, r) ]        
                 guard $ case access t of
                      Leaf False -> False
                      _ -> True
                 return ( p, t )
        (p, t) <- select_one nonempty_children
        Just m <- some_model t
        return $ Just $ M.insert v p m 

-- | list of all models (WARNING not using 
-- variables that had been deleted)
paths :: Ord v => OBDD v -> [ Map v Bool ]
paths = 
  fold ( bool [] [ M.empty ] )
       ( \ v l r -> (M.insert v False <$> l)
                 ++ (M.insert v True  <$> r) )

models vars =
  full_fold vars ( bool [] [ M.empty ] )
       ( \ v l r -> (M.insert v False <$> l)
                 ++ (M.insert v True  <$> r) )
  

select_one :: [a] -> IO a
select_one xs | not ( Prelude.null xs ) = do
    i <- System.Random.randomRIO ( 0, length xs - 1 )
    return $ xs !! i

make :: State ( OBDD v ) Index
     -> OBDD v
make action = 
    let ( i, s ) = runState action empty 
    in  i `seq` s { top = i }

fresh :: State ( OBDD v ) Index
fresh = do
    s <- get
    let i = next s
    put $! s { next = succ i }
    return i

cached :: Ord v
        => (Index, Index) 
        -> ( State ( OBDD v ) Index )
        -> State ( OBDD v ) Index
cached (l,r) action = do
    s <- get
    case IIM.lookup (l, r) $ cache s of
        Just i -> return i
        Nothing -> do
            i <- action
            s <- get
            put $! s { cache = IIM.insert (l, r) i 
                              $ cache s }
            return i

register :: Ord v
         => Node v Index
       -> State ( OBDD v ) Index
register n = case n of
    Leaf False -> return 0
    Leaf True -> return 1
    Branch v l r -> if l == r then return l else do
      s <- get    
      case VIIM.lookup (v, l, r) ( icore s ) of
        Just i -> return i
        Nothing -> do
                i <- fresh
                s <- get
                put $! s 
                    { core = IM.insert i n $ core s
                    , icore = VIIM.insert (v, l, r) i 
                            $ icore s
                    }
                return i

checked_register :: Ord v
         => Node v Index
       -> State ( OBDD v ) Index
checked_register n = case n of
    Branch v l r -> do
      s <- get 
      let check_var_ordering b = case IM.lookup b (core s ) of 
                  Just (Branch av _ _) | not (v > av) -> 
                      error "wrong variable ordering"
                  _ -> return ()
      check_var_ordering l ; check_var_ordering r
      register n
    _ -> register n

-- | Calls the @dot@ executable (must be in @$PATH@) to draw a diagram
-- in an X11 window. Will block until this window is closed.
-- Window can be closed gracefully by typing  'q'  when it has focus.
display :: Show v => OBDD v -> IO ()
display d = void $ readProcess "dot" [ "-Tx11" ] $ toDot d

-- | toDot outputs a string in format suitable for input to the "dot" program
-- from the graphviz suite.
toDot :: (Show v) => OBDD v -> String
toDot (OBDD idmap _ _ top _) =
    unlines [ "digraph BDD {"
            -- Start in oval mode
            , "node[shape=oval];"
            , evalState (helper $ idToNode top) S.empty
            , "}"
            ]
  where
    idToNode =
        let getNode = \i -> case i of
                          0 -> Leaf False
                          1 -> Leaf True
                          _ -> idmap IM.! i
        in id &&& getNode

    unquote s = if isPrefixOf "\"" s && isSuffixOf "\"" s
                then init $ tail s
                else s
    mkLabel lbl = "[label=\"" ++ unquote lbl ++ "\"];"

    helper (thisId, Leaf b) = return $
        -- switch to rectangle nodes for the leaf, before going back to ovals.
        unlines [ "node[shape=rectangle];"
                , show thisId ++ mkLabel (show b)
                , "node[shape=oval];"
                ]
    helper (thisId, Branch vid l r) = do
        -- Ensure we don't traverse children multiple times, if we have more
        -- than one edge into a given node.
        beenHere <- gets (thisId `S.member`)
        if beenHere
            then return ""
            else do
                lstr <- helper $ idToNode l
                rstr <- helper $ idToNode r
                modify (thisId `S.insert`)
                let idStr = show thisId
                return $ unlines
                   [ idStr ++ mkLabel (show vid)
                   , lstr
                   , idStr ++ "->" ++ show l ++ mkLabel "0"
                   , rstr
                   , idStr ++ "->" ++ show r ++ mkLabel "1"
                   ]
