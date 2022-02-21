module Pandora.Pattern.Object.Semigroup (Semigroup (..)) where

infixl 9 +

{- |
> When providing a new instance, you should ensure it satisfies:
> * Associativity: x + (y + z) ≡ (x + y) + z
-}

class Semigroup a where
	{-# MINIMAL (+) #-}
	(+) :: a -> a -> a
