{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- | 

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskell.ThinkingWithTypes.WorkingWithFirstClassFamilies where

import Data.Kind (Constraint, Type)

-- function receive a as parameter
type Exp a = a -> Type

-- type family is class of type level
type family Eval (e :: Exp a) :: a

-- Exp is shell, Pure give a shell to you
data Pure :: a -> Exp a
-- type instance is instance of type level
type instance Eval (Pure x) = x

-- {-# LANGUAGE TypeOperators #-}
-- just like monad operation =<< in type level, deshell to enter function
-- =< means enter the shell, < means call function ,input to output
data (=<<) :: (a -> Exp b) -> Exp a -> Exp b 
type instance Eval (k =<< e) = Eval (k (Eval e))
infixr 0 =<<

-- just like function compostion at the type level
-- two < means two fucntions serial application
data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c

type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))
infixr 1 <=<

data TyEq :: a -> b -> Exp Bool
type instance Eval (TyEq a b) = TyEqImpl a b
type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

-- just like unfold the list to muple (more than tuple)
-- more exactly, unfold the Constraint list to Constraint muple in Exp shell
data Collapse :: [Constraint] -> Exp Constraint
type instance Eval (Collapse '[]) = (() :: Constraint)
type instance Eval (Collapse (a ': as  )) = (a, Eval (Collapse as))

-- say here All series is type ,would rather say it is a type level fuction or routine to generate type level Constrain expression
type All (c :: k -> Constraint) (ts :: [k]) =
  Collapse =<< MapList (Pure1 c) ts
-- MapList to generate  Constraint list in Exp shell,then fmap Collapse into Exp shell,unfold this Constraint list to Consraint muple in Exp shell still

type Hello = All Eq '[Int,Bool]
type Hello1 = Hello (Show Int)

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x 

data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[] ) = '[]
type instance Eval (MapList f (a ': as )) =
  Eval (f a)  ': Eval (MapList f as)  -- actually : is ok too

data HList (ts :: [Type]) where
  HNil :: HList '[]
  --(:#) :: t -> HList ts -> HList (t ': ts)
  Link :: t -> HList ts -> HList (t ': ts)
infixr 5 `Link` -- :#

-- Show ts is Constraint, instance "Constraint" => ...
-- Eval (All Eq ts) is Constraint too
instance Eval (All Eq ts) => Eq (HList ts) where
  HNil == HNil = True
  (a `Link` as) == (b `Link` bs) = a == b && as == bs

-- this is orginal implementation in Chapter 5,see TypeFamiliesConstrains.hs
-- type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
--   All c '[] = ()
--   All c (t ': ts) = (c t, All c ts)
-- instance All Eq ts => Eq (HList ts) where
--   HNil == HNil = True
--   (a `Link` as) == (b `Link` bs) = a == b && as == bs
