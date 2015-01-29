{-# language ScopedTypeVariables #-}

module OBDD.Operation 

( (&&), (||), not, and, or
, unary, binary
, instantiate
, exists, exists_many
, fold, foldM
)

where

import OBDD.Data
import OBDD.Make

import qualified Data.List ( sortBy)
import Data.Function (on)

import Data.Set ( Set )
import qualified Data.Set as S

-- import Data.List ( foldl' )
-- don't use, see below

import Prelude hiding ( (&&), (||), and, or, not )
import qualified Prelude

( && ) :: Ord v => OBDD v -> OBDD v -> OBDD v
( && ) = binary ( Prelude.&& )

( || ) :: Ord v => OBDD v -> OBDD v -> OBDD v
( || ) = binary ( Prelude.|| )



and :: Ord v => [ OBDD v ] -> OBDD v
and = fold_by_size (constant True) (&&)
-- and = foldr ( && ) ( constant True ) 
-- writing foldl or fold' here 
-- makes performance MUCH worse!

or :: Ord v => [ OBDD v ] -> OBDD v
or = fold_by_size (constant False) (||)
-- or = foldr ( || ) ( constant False ) 

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


-- | FIXME this is a silly implementation. Negation should be done
-- by switching values in Leaves (?)
not :: Ord v => OBDD v -> OBDD v 
not = unary ( Prelude.not )

unary :: Ord v 
      => ( Bool -> Bool )
      -> OBDD v -> OBDD v
unary op x = make $ do
    let handle x = cached ( top x, top x ) $ case access x of
                Leaf p -> register $ Leaf $ op p
                Branch v l r -> do
                        l' <- handle l
                        r' <- handle r
                        register $ Branch v l' r'
    handle x


binary :: Ord v
      => ( Bool -> Bool -> Bool )
      -> OBDD v -> OBDD v -> OBDD v
binary op x y = make $ do
    let -- register = checked_register -- for testing
        handle x y = cached (top x, top y) $ case ( access x , access y ) of
                    ( Leaf p , Leaf q ) -> register $ Leaf $ op p q
                    ( ax, ay ) -> case comp ax ay of
                        LT -> do
                            let Branch v l r = ay
                            l' <- handle x l
                            r' <- handle x r
                            register $ Branch v l' r'
                        GT -> do
                            let Branch v l r = ax
                            l' <- handle l y
                            r' <- handle r y
                            register $ Branch v l' r'
                        EQ -> do
                            let Branch v1 l1 r1 = ax
                                Branch v2 l2 r2 = ay
                                v = if v1 == v2 then v1 else error "OBDD.Operation.handle"
                            l' <- handle l1 l2
                            r' <- handle r1 r2
                            register $ Branch v l' r'
    handle x y

-- | remove variables existentially
-- TODO: needs better implementation
exists_many :: Ord v 
            => Set v
            -> OBDD v
            -> OBDD v
exists_many vars x =
    foldr exists x $ S.toList vars 

-- | remove variable existentially
-- TODO: needs better implementation
exists :: Ord v
      => v
      -> OBDD v -> OBDD v
exists var x = 
    instantiate var False x || instantiate var True x

-- | replace variable by value
instantiate :: Ord v => 
            v -> Bool 
            -> OBDD v
            -> OBDD v
instantiate var val x = make $ do
    let handle x = cached ( top x, top x ) $ case access x of
            Leaf p -> register $ Leaf p
            Branch v l r -> 
                if v == var then do
                     let t = if val then r else l
                     handle t
                else  do
                     l' <- handle l
                     r' <- handle r
                     register $ Branch v l' r' 
    handle x

comp x y = case (x , y) of
    ( Leaf {} , Leaf {} ) -> EQ
    ( Branch {} , Leaf {} ) -> GT
    ( Leaf {} , Branch {} ) ->  LT
    ( Branch v1 _ _ , Branch v2 _ _ ) -> compare v1 v2
    
