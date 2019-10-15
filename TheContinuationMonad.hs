-- |
{-# LANGUAGE RankNTypes #-}

module Haskell.ThinkingWithTypes.TheContinuationMonad where

import Control.Monad.Trans.Class

-- having a function which give(pass) 'a' to another function
cont :: a -> (forall r. (a -> r) -> r)
cont a ar = ar a

-- having 'a'
runCont :: (forall r. (a -> r) -> r) -> a
runCont arr = arr id

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }
  
-- newtype EitherT e m a =
--  EitherT {runEitherT :: m (Either e a)}
  
newtype ContT m a =
  ContT {runContT :: m (Cont a)}

instance Functor Cont where
  -- lanmbda way
  -- fmap ab uC = Cont $ \br ->  br ((unCont uC) ab)
  -- point free way, $ is a infix operator, so both ($ 3) and ((12+) $) are permitted
  fmap ab uC = Cont $ ($ arr ab) where
    arr = unCont uC

instance Applicative Cont where
  pure a = Cont $ ($ a)
  cTab <*> cTa = fmap (abrr id) cTa  where
    abrr = unCont cTab

instance Monad Cont where
  return = pure
  cTa >>= aCTb = aCTb a where
    a = unCont cTa id


instance MonadTrans ContT where
  lift ma = ContT $ do
    a <- ma
    return $ return a

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0
withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362
withOS :: (String -> r) -> r
withOS f = f "linux"

releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
  version <- Cont withVersionNumber
  date <- Cont withTimestamp
  os <- Cont withOS
  return $ os ++ "-" ++ show version ++ "-" ++ show date
