module Pattern.Object.Monoid (Monoid (..)) where

import Pattern.Object.Semigroup (Semigroup)

{- |
> When providing a new instance, you should ensure it satisfies the two law:
> * Right absorption: unit <> x ≡ x
> * Left absorption: x <> unit ≡ x
-}

class Semigroup a => Monoid a where
	{-# MINIMAL unit #-}
	unit :: a
