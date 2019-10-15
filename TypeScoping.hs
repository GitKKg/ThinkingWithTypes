{-# LANGUAGE ScopedTypeVariables #-}
-- | 

module Haskell.ThinkingWithTypes.TypeScoping where


broken :: (a -> b) -> a -> b
broken f a = apply
  where
    -- apply :: b
    apply = f a

working :: forall a b. (a -> b) -> a -> b
working f a = apply
  where
    apply :: b
    apply = f a
