module Pandora.Pattern.Object.Semigroup (Semigroup (..)) where

infixl 6 +

{- |
> When providing a new instance, you should ensure it satisfies the one law:
> * Associativity: x + (y + z) ≡ (x + y) + z
-}

class Semigroup a where
	{-# MINIMAL (+) #-}
	(+) :: a -> a -> a
