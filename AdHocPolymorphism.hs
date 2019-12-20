{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
-- | 

module Haskell.ThinkingWithTypes.AdHocPolymorphism where


import Data.Kind (Constraint, Type)
-- function receive a as parameter
type Exp a = a -> Type

-- type family is class of type level
type family Eval (e :: Exp a) :: a

data Map :: (a -> Exp b ) -> f a -> Exp (f b)

type instance Eval (Map f '[] ) = '[]

-- ': need  {-# LANGUAGE TypeOperators #-}
type instance Eval (Map f (a ': as  )) = Eval (f a) ': Eval (Map f as)

type instance Eval (Map f 'Nothing ) = 'Nothing
type instance Eval (Map f ( 'Just a )) = 'Just (Eval (f a))

type instance Eval (Map f  ('Left x ) ) = 'Left x
type instance Eval (Map f ('Right a  )) = 'Right (Eval (f a))

type instance Eval (Map f '(a ,b) ) = '( a, Eval (f b)) 

data Mappend :: a -> a -> Exp a
type instance Eval (Mappend  '() '() ) = '()
type instance Eval (Mappend (a :: Constraint) (b :: Constraint)  ) = (a, b)
type instance Eval (Mappend (a :: [k]) (b :: [k] )) =  a ++ b

type family (++)  (la :: [a])   (lb :: [a])  :: [a] where
  (a:la) ++ (b:lb) = (a : '[ ]) ++ la ++ (b : '[]) ++ lb

data Mempty :: k -> Exp k

type instance Eval (Mempty '() ) = '()
type instance Eval (Mempty (c :: Constraint)) = (() :: Constraint)
type instance Eval (Mempty (l :: [k])) = '[]
