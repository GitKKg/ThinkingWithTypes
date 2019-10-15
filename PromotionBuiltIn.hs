{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds ,TypeOperators #-}

module Haskell.TypeLearning.PromotionBuiltIn where

import GHC.TypeLits

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = y

type family Not (x :: Bool) :: Bool where
  Not 'False = 'True
  Not 'True = 'False

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

data UserType =
  User
  | Admin

-- kind Bool =
  -- 'True
  -- | 'False

-- type family Proxy = Proxy '[TYPE]  



