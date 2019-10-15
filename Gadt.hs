{-# LANGUAGE GADTs #-}
-- | 

module Haskell.ThinkingWithTypes.Gadt where

data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

getMaybe :: Maybe a -> a
getMaybe (Just a) = a
