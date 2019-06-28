module Pandora.Pattern.Object.Monoid (Monoid (..)) where

import Pandora.Pattern.Object.Semigroup (Semigroup)

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Right absorption: unit + x ≡ x
> * Left absorption: x + unit ≡ x
-}

class Semigroup a => Monoid a where
	{-# MINIMAL unit #-}
	unit :: a
