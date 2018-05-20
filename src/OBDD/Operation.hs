{-# language ScopedTypeVariables #-}
{-# language PatternGuards #-}
{-# language FlexibleContexts #-}

module OBDD.Operation 

( Boolean(..)
, equiv
, unary, binary
, instantiate
, exists, exists_many
, forall, forall_many
, fold, foldM
, full_fold, full_foldM
)

where

import OBDD.Data
import OBDD.Make
import Ersatz.Bit 
import Data.Foldable (toList)

import qualified Data.IntMap as IM
import qualified OBDD.IntIntMap as IIM
import qualified Data.Map as M

import qualified Control.Monad.State.Strict as T

import qualified Data.List ( sortBy)
import Data.Function (on)

import Data.Set ( Set )
import qualified Data.Set as S

import Prelude hiding ( (&&), (||), and, or, not, any, all, bool )
import qualified Prelude
import qualified Data.Bool

instance Ord v => Boolean (OBDD v) where
  bool f = constant f
  not = not_
  ( && ) = symmetric_binary ( Prelude.&& )
  ( || ) = symmetric_binary ( Prelude.|| )
  choose no yes sel =
    -- https://github.com/jwaldmann/haskell-obdd/issues/10
    (no && not sel) || (yes && sel)
  xor   = symmetric_binary (/=)
  ( ==> ) = binary ( <= )
  all p = fold_by_size true  (&&) . map p . toList
  any p = fold_by_size false (||) . map p . toList

equiv :: Ord v => OBDD v -> OBDD v -> OBDD v
equiv = symmetric_binary (==)

fold_by_size base reduce fs =
    let handle fs = case fs of
            [] -> base
            [f1 ] -> f1
            f1 : f2 : rest -> 
                let f = reduce f1 f2
                in  handle $ insert f rest
        insert f gs = case gs of
            g : hs | size f > size g -> g : insert f hs
            _ -> f : gs
    in  handle $ Data.List.sortBy (compare `on` size) fs


-- | FIXME: this function is nonsensical. There is only one interesting
-- unary Boolean function (negation), and it should be handled differently.
unary :: Ord v 
      => ( Bool -> Bool )
      -> OBDD v -> OBDD v
unary op x = make $ do
    let handle x = cachedIM (top x) $ case access x of
                Leaf p -> do
                  T.lift $ register $ Leaf $ op p
                Branch v l r -> do
                        l' <- handle l
                        r' <- handle r
                        T.lift $ register $ Branch v l' r'
    flip T.evalStateT IM.empty $ handle x


data Symmetricity = Asymmetric | Symmetric deriving Show

binary :: Ord v
      => ( Bool -> Bool -> Bool )
      -> OBDD v -> OBDD v -> OBDD v
binary = binary_ Asymmetric
      
symmetric_binary :: Ord v
      => ( Bool -> Bool -> Bool )
      -> OBDD v -> OBDD v -> OBDD v
symmetric_binary = binary_ Symmetric


binary_ :: Ord v
      => Symmetricity
      -> ( Bool -> Bool -> Bool )
      -> OBDD v -> OBDD v -> OBDD v

-- FIXME https://github.com/jwaldmann/haskell-obdd/issues/4
binary_ _ op x y = make $ do
    let -- register = checked_register -- for testing
        -- handle x y | Symmetric <- sym, top x > top y = handle y x
        handle x y = cachedIIM (top x, top y) $ case ( access x , access y ) of
                    ( Leaf p , Leaf q ) -> do
                        T.lift $ register $ Leaf $ op p q
                    ( ax, ay ) -> case comp ax ay of
                        LT -> do
                            let Branch v l r = ay
                            l' <- handle x l
                            r' <- handle x r
                            T.lift $ register $ Branch v l' r'
                        GT -> do
                            let Branch v l r = ax
                            l' <- handle l y
                            r' <- handle r y
                            T.lift $ register $ Branch v l' r'
                        EQ -> do
                            let Branch v1 l1 r1 = ax
                                Branch v2 l2 r2 = ay
                                v = if v1 == v2 then v1 else error "OBDD.Operation.handle"
                            l' <- handle l1 l2
                            r' <- handle r1 r2
                            T.lift $ register $ Branch v l' r'
    flip T.evalStateT IIM.empty $ handle x y

cachedM arg act = do
  c <- T.get
  case M.lookup arg c of
    Nothing -> do
      res <- act ; T.modify' $ M.insert arg res; return res
    Just res -> do
      return res

cachedIM arg act = do
  c <- T.get
  case IM.lookup arg c of
    Nothing -> do
      res <- act ; T.modify' $ IM.insert arg res; return res
    Just res -> do
      return res

cachedIIM arg act = do
  c <- T.get
  case IIM.lookup arg c of
    Nothing -> do
      res <- act ; T.modify' $ IIM.insert arg res; return res
    Just res -> do
      return res

comp x y = case (x , y) of
    ( Leaf {} , Leaf {} ) -> EQ
    ( Branch {} , Leaf {} ) -> GT
    ( Leaf {} , Branch {} ) ->  LT
    ( Branch v1 _ _ , Branch v2 _ _ ) -> compare v1 v2

-- | remove variables existentially
exists_many :: (Foldable c, Ord v)
            => c v
            -> OBDD v
            -> OBDD v
exists_many vars x =
    foldr exists x vars 

-- | remove variable existentially
exists :: Ord v
      => v
      -> OBDD v -> OBDD v
exists var = fold bool
  ( \ v l r -> if var == v then l || r else choose l r (variable v) )

forall_many :: (Foldable c, Ord v) => c v -> OBDD v -> OBDD v
forall_many vars x = 
    foldr forall x vars 

forall :: Ord v => v -> OBDD v -> OBDD v
forall var = fold bool
  ( \ v l r -> if var == v then l && r else choose l r (variable v) )

-- | replace variable by value
instantiate :: Ord v => 
            v -> Bool 
            -> OBDD v
            -> OBDD v
instantiate var val = fold bool
  ( \ v l r ->
    if var == v then Data.Bool.bool l r val
    else choose l r (variable v) )

    
