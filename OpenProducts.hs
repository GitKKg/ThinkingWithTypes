-- | 
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskell.ThinkingWithTypes.OpenProducts where
import Data.Coerce (Coercible(..), coerce)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Fcf

data Any (f :: k -> Type) where
  Any :: f t -> Any f

-- two lists here,one is type list ts, another one is Vector to store data at term level
data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts
-- in One OpenProduct variable,f is always fixed ,but k in ts could be any type

nil :: OpenProduct f '[]
nil = OpenProduct V.empty
-- f here is not V.Vector

data Key (key :: Symbol) = Key

key = Key @"myData"
-- key :: Key "myData"

insert0 :: Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)

insert0 _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

opV = nil
opVn = insert0 key [] opV

result = insert0 (Key @"key") (Just "hello") opV
-- f here is Maybe , ts here is '[ '("key", [Char])]

result2 = insert0 (Key @"another") (Just True) result
-- f here is Maybe , ts here is '[ '("another", Bool), '("key", [Char])]

-- data (=<<) (c :: a -> Exp b) (d :: Exp a) (e :: b)
--   	-- Defined in ‘Fcf’
-- infixr 1 =<<
-- type instance Eval (k =<< e) = Eval (k (Eval e))
-- data (<=<) (d :: b -> Exp c) (e :: a -> Exp b) (f :: a) (g :: c)
-- type instance Eval ((<=<) f g x) = Eval (f (Eval (g x)))
-- data Filter (b :: a -> Exp Bool) (c :: [a]) (d :: [a])
-- type instance Eval (Filter _p '[]) = '[] 	-- Defined in ‘Fcf’
-- type instance Eval (Filter p (a2 : as))
--   = If (Eval (p a2)) (a2 : Eval (Filter p as)) (Eval (Filter p as))
--   	-- Defined in ‘Fcf’
-- data Fst (c :: (a, b)) (d :: a)
--   	-- Defined in ‘Fcf’
-- type instance Eval (Fst '(a2, _b)) = a2 	-- Defined in ‘Fcf’
-- type family If (b :: Bool) (x :: k) (y :: k) :: k
--   	-- Defined in ‘Fcf’
-- type instance If 'False _x y = y 	-- Defined in ‘Fcf’
-- type instance If 'True x _y = x 	-- Defined in ‘Fcf’
type UniqueKey (key :: k) (ts :: [(k, t)])
  = Null =<< Filter (TyEq key <=< Fst) ts
-- :kind   , below * get no sense? yes, c -> * means Exp c
-- (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> c -> *
-- TyEq :: a -> b -> Bool -> *
-- Filter :: (a -> Exp Bool) -> [a] -> [a] -> *
-- Fst :: (a, b) -> a -> *
-- Null :: [a] -> Bool -> *
-- (=<<) :: (a -> Exp b) -> Exp a -> b -> *
-- equivalent of null . filter (== key) . fst

insert
  :: Eval (UniqueKey key ts) ~ 'True -- this key is Unique in ts ,it's true(in type level,must 'True)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)

insert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v


-- new = insert ( Key @"key") ( Just True ) result2
-- not pass, "key" is not UniqueKey

newRe = insert ( Key @"keyNew") ( Just True ) result2
-- pass , "keyNew" is UniqueKey

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)
-- (=<<) :: (a -> Exp b) -> Exp a -> b -> *,  deshell into math then deshell agian

-- KnownNat :: Nat -> Constraint
-- FindElem :: Symbol -> [(Symbol, k)] -> Nat
findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)
-- natVal :: KnownNat n => proxy n -> Integer

-- it's not type family, FromMaybe is not too
type LookupType (key :: k) (ts :: [(k, t)]) =
  FromMaybe Stuck =<< Lookup key ts
-- Lookup :: k -> [(k, b)] -> Exp (Maybe b)
-- :k FromMaybe
-- FromMaybe :: k -> Maybe k -> k -> *
-- but in TypeLevelDefunctionalization.hs , data FromMaybe :: a -> Maybe a -> Exp a
-- these two should be the same thing, but how?
-- because type Exp a = a -> *
-- so LookupType will get Exp t, just like LookupType :: k -> [(k, t)] -> t -> *  shows

get :: forall key ts f. KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> f (Eval (LookupType key ts))
get _ (OpenProduct v) =
  unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a
-- V.unsafeIndex :: V.Vector a -> Int -> a
-- findElem find index number in ts which key corresponds
-- then V.unsafeIndex receive this index to get element from V.Vector
-- finally unsafeCoerce convert this Any type element to real type which wrap
-- actually issue still exist, f here is fixed, how deshell it to get variable t inside in gerneal type function way?
-- write a function \(f a) -> a ?


type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  SetIndex (FindElem key ts) '(key, t) ts
-- SetIndex :: Nat -> a -> [a] -> [a] -> *

update :: forall key ts t f. KnownNat (FindElem key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpdateElem key t ts))

update _ ft (OpenProduct v) =
  OpenProduct $ v V.// [(findElem @key @ts, Any ft)]
-- (V.//) :: V.Vector a -> [(Int, a)] -> V.Vector a
-- here V.// is a operator

--type DeleteElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
--  SetIndex (FindElem key ts) '(key, t) ts


-- delete element whose key is key
delete :: forall key ts t f. KnownNat (FindElem key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpdateElem key t ts))
delete _ ft (OpenProduct v) =
  OpenProduct $  fst vg V.++ (V.tail . snd) vg where
  vg = V.splitAt (findElem @key @ts) v


-- type family GetIndex (k :: Symbol) (ts :: [(Symbol , val)]) :: Maybe Nat
type  GetIndex  key  ts = If (Eval (UniqueKey key ts)) Nothing  (Just (FindElem key ts))
--type  GetIndex  key  ts = If (Eval (UniqueKey key ts))  ('Just  (FindElem key ts)) 'Nothing
-- :kind! GetIndex "hi" '[ '( "what" ,8  ), '("hi",12) ]
-- GetIndex "hi" '[ '( "what" ,8  ), '("hi",12) ] :: Maybe Nat
-- = 'Just 1

-- cp from monadplus in Github, just misundetstand what kind to term-level means
-- Lower the kind to the term-level
-- use class to conver kind,type level to term in type
class FindUpsertElem (a :: Maybe Nat) where -- class could accept kind as parameter, just try to remember it just class do ,it just a Consraint
  upsertElem :: Maybe Int
-- :k FindUpsertElem
-- FindUpsertElem :: Maybe Nat -> Constraint

instance FindUpsertElem 'Nothing where -- instance could accept different type parameters explicitily and directly
  upsertElem = Nothing
instance KnownNat nat => FindUpsertElem ('Just nat) where -- instance could accept different type parameters explicitily and directly
  upsertElem = Just (fromIntegral $ natVal (Proxy @nat))

-- Recompute ts
type UpsertElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)])
 = FromMaybe ('(key, t) ': ts)
   =<< Map (SetIndex2 '(key, t) ts)
   =<< FindIndex (TyEq key <=< Fst) ts
-- FindIndex :: (a -> Exp Bool) -> [a] -> Exp (Maybe Nat)
-- data (=<<) :: (a -> Exp b) -> Exp a -> Exp b infixr 1 
-- data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c infixr 1, no deshell at first,but deshell at the second step
-- Map :: (a -> Exp b) -> f a -> Exp (f b)
-- FromMaybe :: k -> Maybe k -> Exp k

-- Same as SetIndex but with reversed arguments.
data SetIndex2 :: a -> [a] -> Nat -> Exp [a]
type instance Eval (SetIndex2 a' as n) = SetIndexImpl n a' as
-- SetIndexImpl here is just off the book, type family in Fcf
-- SetIndexImpl :: Nat -> k -> [k] -> [k]

upsert
  :: forall key ts t f
   . FindUpsertElem (GetIndex key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpsertElem key t ts))
upsert _ ft (OpenProduct v) =
  OpenProduct $ case upsertElem @(GetIndex key ts) of
    Nothing -> V.cons (Any ft) v
    Just i  -> v V.// [(i, Any ft)]

-- :set -XTypeApplications
-- :set -XOverloadedLabels
--  :t #foo
-- #foo :: IsLabel "foo" t => t
--  :t fromLabel @"foo"
-- fromLabel @"foo" :: IsLabel "foo" a => a

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key
