module Pandora.Pattern.Object.Group (Group (..)) where

import Pandora.Pattern.Object.Monoid (Monoid)

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Right absorption: x + inverse x ≡ zero
> * Left absorption: inverse x + x ≡ zero
-}

class Monoid a => Group a where
	{-# MINIMAL inverse #-}
	inverse :: a -> a
