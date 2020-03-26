module Pandora.Pattern.Object.Ringoid (Ringoid (..)) where

import Pandora.Pattern.Object.Semigroup (Semigroup)

infixl 7 *

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Left distributivity: x * (y + z) ≡ x * y + x * z
> * Right distributivity: (y + z) * x ≡ y * x + z * x
-}

class Semigroup a => Ringoid a where
	{-# MINIMAL (*) #-}
	(*) :: a -> a -> a
