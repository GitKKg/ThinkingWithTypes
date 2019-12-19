
-- | 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskell.ThinkingWithTypes.Defunctionalization where

import Prelude hiding (fst)
-- just hide some symbol ,here ,fst function
-- for reimplemetion

fst :: (a ,b) -> a
fst (a, b) = a

data Fst a b = Fst (a, b)

class Eval l t | l -> t where -- {-# LANGUAGE FunctionalDependencies #-}
  eval :: l -> t

instance Eval (Fst a b) a where
  eval (Fst (a, b)) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] =  Nothing
listToMaybe la = Just  $ head la

data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe []) = Nothing
  eval (ListToMaybe la) = Just $ head la

data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList f []) = []
  eval (MapList f (a : as)) = eval (f a) : eval (MapList f as) -- f a is dfb ,is eval able

aa = eval (Fst ("hello",True))
-- "hello"

data FunOp fb a = FunOp (a -> fb) a
instance Eval fb fa => Eval (FunOp fb a) fa where
  eval (FunOp f a) = eval (f a)
