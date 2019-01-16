module Pattern.Object.Group (Group (..)) where

import Pattern.Object.Monoid (Monoid)

{- |
> When providing a new instance, you should ensure it satisfies the two law:
> * Right absorption: x <> inverse x ≡ unit
> * Left absorption: inverse x <> x ≡ unit
-}

class Monoid a => Group a where
	{-# MINIMAL inverse #-}
	inverse :: a -> a
