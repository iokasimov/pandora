module Pandora.Pattern.Object.Chain (Chain (..)) where

import Pandora.Pattern.Object.Setoid (Setoid)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering, order)

{- |
> When providing a new instance, you should ensure it satisfies the three laws:
> * Reflexivity: x <= x ≡ True
> * Transitivity: x <= y && y <= z ≡ True ===> x <= z ≡ True
> * Antisymmetry: x <= y && y <= x ≡ True ===> x == y ≡ True
-}

class Setoid a => Chain a where
	{-# MINIMAL (<=>) #-}
	(<=>) :: a -> a -> Ordering

	(<) :: a -> a -> Boolean
	x < y = order True False False (x <=> y)
	(<=) :: a -> a -> Boolean
	x <= y = order True True False (x <=> y)
	(>) :: a -> a -> Boolean
	x > y = order False False True (x <=> y)
	(>=) :: a -> a -> Boolean
	x >= y = order False True True (x <=> y)
