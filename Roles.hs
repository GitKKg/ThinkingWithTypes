{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
-- | 
{-# LANGUAGE RoleAnnotations #-}

module Haskell.ThinkingWithTypes.Roles where

import Data.Coerce (Coercible(..), coerce)
import Data.Monoid (Sum (..), Product (..))

as = Sum 2 :: Sum Int
bs = Sum 3 :: Sum Integer

-- cs = coerce bs :: Sum Int
five_ :: (a ~ Int) => a
five_ = 5

-- refe to GADTs chapter
-- what use for ~
-- (~) :: k -> k -> Constraint
data Expr_ a =
  (a ~ Int) => LitInt_ Int | (a ~ Bool) => LitBool_ Bool | (a ~ Int) => Add_ (Expr_ Int) (Expr_ Int) | (a ~ Bool) => Not_ (Expr_ Bool) | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)
-- '|' must stay with one line

ea = Left 12 :: Either Integer Integer
-- eb = Right 12 :: Either Integer Integer

esa = Left (Sum 12) :: Either (Sum Integer) Integer

esaea = coerce ea :: Either Integer Integer
-- sa = coerce ea :: Either (Sum Integer) String -- not pass
-- so ,seem a of Either a is nominal role

esb = Right  12 :: Either (Sum Integer) Integer

esbbs = coerce esb :: Either Integer Integer

essb = Right (Sum 12) :: Either (Sum Integer) (Sum Integer)

essbbssb = coerce essb :: Either Integer Integer
-- sb = coerce essb :: Either String Integer  --not pass
-- so seem b of Either a b is nomianl role

  
data Ei a b =
  Le a | Ri b

-- not pass
-- a = coerce ea :: Integer
-- la = coerce ea :: Ei Integer Integer


ssa = Sum "ok"
ass = coerce ssa :: String

data Proxy a = Proxy

pa = Proxy :: Proxy Integer

ap = coerce pa :: Proxy String -- pass

-- so , a of Proxy a is phatom role


type family IntToBool a where
  IntToBool Int = Bool
  IntToBool a = a

data BST v
  = Empty
  | Branch (BST v) v (BST v)


type role BST nominal -- v now is nominal
