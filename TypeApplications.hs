-- |
{-#LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Haskell.ThinkingWithTypes.TypeApplications where


constM :: a -> b -> a
constM a b = a

constInt = constM @Int

-- dig = constInt "p" "notOK"
dig = constInt 12 "ok"


constM2 :: forall b a. a -> b -> a
constM2 a b = a

constInt2 = constM2 @Int

-- dig3 = constInt2 "notOK" "12"
dig2 = constInt2 "ok" 12

constInt3 = constM @_ @Int
dig3 = constInt3 "ok" 12
-- dig4 = constInt3 "notOK" "12"

