{-# LANGUAGE RankNTypes #-}
-- | 

module Haskell.ThinkingWithTypes.RankNtypes where


applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

-- Exercise 6.3-i
e6p3i :: Int -> forall a. a -> a
e6p3i i = id

-- order 2 ?
rarr :: forall r. ((forall a. (a -> r)) -> r)
rarr ar = ar "anything"

-- order 2 ?
e6p3ii :: (a -> b) -> (forall c. c -> a) -> b
e6p3ii fab ca = fab (ca "anything")

-- order 3 ?
-- e6p3iii :: ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
-- e6p3iii mxbzmx (b (zma)) =  mxbzmx (m id)  
