-- | 

module Haskell.ThinkingWithTypes.Variance where

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 ia) = T1 ( f . ia)


newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap ab (T5 aii) =  T5 (\bi -> aii (bi . ab)) 

newtype T4 a = T4 ((Int -> a) -> Int)

-- instance Functor T4 where
  -- fmap f (T4 iai) = T4 ()
