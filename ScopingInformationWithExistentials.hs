{-# LANGUAGE RankNTypes #-}
-- | 

module Haskell.ThinkingWithTypes.ScopingInformationWithExistentials where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable
import Control.Monad.Trans
import System.IO
newtype ST s a = ST
  {
    unsafeRunST :: a
  }

sti = ST 12
tw = unsafeRunST sti

stii = ST 12 :: ST Integer Integer
-- :t stii
-- stii :: ST Integer Integer

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a -- seq :: a -> b -> b, gurrantee a be valued before b

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
  ST a >>= f = seq a $ f a

-- seq is frequently used with accumulating parameters to ensure that they don't become huge thunks, which will be forced at the end anyway. For example, strict foldl:

-- huge thunks means huge memory allocation and potential memory exhausted crash
-- expreesion being valued means memory occuption relieved

-- foldl' :: (a -> b -> a) -> a -> [b] -> a
-- foldl' _ z [] = z
-- foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- It's also used to define strict application:

-- ($!) :: (a -> b) -> a -> b
-- f $! x = x `seq` f x

instance MonadIO (ST s) where
  liftIO ioa  = do
    pure ioa
    pure $ unsafePerformIO ioa
    
     
newtype STRef s a = STRef
--s acts as a label irrevocably knotting a STRef with the ST context that created it
-- ie.: ST s (STRef s a)
  {
    unSTRef :: IORef a
  }
  
-- construct a STRef
strI = STRef $ unsafePerformIO $ newIORef 12 :: STRef String Integer
-- newIORef :: a -> IO (IORef a)
-- unsafePerformIO :: IO a -> a  -- just deshell the shell IO 
-- readIORef :: IORef a -> IO a
-- writeIORef :: IORef a -> a -> IO ()
newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

stSTRi = newSTRef 12 -- STRef is embeded in ST

-- use your own understanding to describe the defition,conception,here , ie. this function
-- read from data a in STRef into ST, in a ST context
readSTRef :: STRef s a -> ST s a
readSTRef =
  pure . unsafePerformIO . readIORef . unSTRef

rStSTRi = readSTRef . unsafeRunST $ stSTRi -- :: ST s Integer
aInSTsa = unsafeRunST rStSTRi

-- write with data a into STRef, in a ST context
writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref =
  pure . unsafePerformIO . writeIORef (unSTRef ref)

newAinSTRef =unsafeRunST $ (writeSTRef . unsafeRunST $ stSTRi)  16 >> (readSTRef . unsafeRunST) stSTRi 
-- now should be 16

-- modify data a in STRef with function (a -> a), in a ST context
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref $ f a

runST :: (forall s. ST s a) -> a  -- forall: {-# LANGUAGE RankNTypes #-}
runST = unsafeRunST

modifiedAinSTRef = runST $ (flip modifySTRef (+18) . unsafeRunST $ stSTRi)  >> rStSTRi
-- now should be 30
-- runST not ok in unsafeRunST postion,must use unsafeRunST ,because modifySTRef's signature dose not accept forall,but why first runST ok?

stSTRiRigid  = newSTRef 12 :: ST String (STRef String Integer) -- must sepify exact type.  STRef is embeded in ST
rStSTRiRigid = readSTRef . unsafeRunST $ stSTRiRigid -- :: ST s Integer
modifiedAinSTRefRigid :: IO ()
modifiedAinSTRefRigid = print  modifyV >> print typeModifyV >> print typeSTRef where
  modifyRead = (flip modifySTRef (+18) . unsafeRunST $ stSTRiRigid)  >> rStSTRiRigid
  typeSTRef = typeOf modifyRead
  typeModifyV = typeOf modifyV
  modifyV = unsafeRunST modifyRead


opFile str = do
  writeFile "/home/kyle/Haskell/file.txt" str

-- modifySTRef :: STRef s a -> (a -> a) -> ST s ()
liftModifiedAinSTRefRigid ::   ST String  ()
liftModifiedAinSTRefRigid =  stSTRiRigid >>= modify >> stSTRiRigid >>= modify >> ( liftIO . print . show . unsafeRunST)  rStSTRiRigid  where -- replace opFile with print ,same phnomenon
--liftModifiedAinSTRefRigid =  stSTRiRigid >>= modify >> stSTRiRigid >>= modify >> seq <$>  ( liftIO . print) <*> (liftIO . opFile)  $ (show . unsafeRunST)  rStSTRiRigid  where
  modify = flip modifySTRef (+18)
  readRef = readSTRef . unsafeRunST
  -- typeSTRef = typeOf modify
  -- typeModifyV = typeOf modifyV
  -- modifyV = unsafeRunST modify

-- modifiedAinSTRefRigid
-- 30
-- ST [Char] Integer

safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ " world")
  readSTRef ref

main :: IO ()
main = do
  return $ unsafeRunST liftModifiedAinSTRefRigid -- file saving no effect
  print $ unsafeRunST liftModifiedAinSTRefRigid
  print $ unsafeRunST safeExample
  -- file saving take effct, but only after  execution of main in the first time, should it because the variable just be evaluated once when required, so IO here takes effect only once?

-- :t runST
-- runST :: (forall s. ST s a) -> a
-- :t unsafeRunST
-- unsafeRunST :: ST s a -> a
-- let a = runST (newSTRef True) -- would not pass
  let b= unsafeRunST (newSTRef True) -- pass
  return ()

-- main
--"48"
-- ()
-- main
-- ()
