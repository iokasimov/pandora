module Pandora.Pattern.Object.Group (Group (..)) where

import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid)

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Right absorption: x + invert x ≡ zero
> * Left absorption: invert x + x ≡ zero
-}

class Monoid a => Group a where
	{-# MINIMAL invert #-}
	invert :: a -> a

	(-) :: a -> a -> a
	x - y = x + invert y
