module Pandora.Pattern.Object.Monoid (Monoid (..)) where

import Pandora.Pattern.Object.Semigroup (Semigroup)

{- |
> When providing a new instance, you should ensure it satisfies:
> * Right absorption: zero + x ≡ x
> * Left absorption: x + zero ≡ x
-}

class Semigroup a => Monoid a where
	{-# MINIMAL zero #-}
	zero :: a
