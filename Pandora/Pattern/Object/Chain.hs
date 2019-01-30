module Pandora.Pattern.Object.Chain (Ordering (..), order, Chain (..)) where

import Pandora.Core.Morphism (($))
import Pandora.Pattern.Object.Setoid (Boolean (True, False), Setoid)

data Ordering = Less | Equal | Greater

order :: a -> a -> a -> Ordering -> a
order x _ _ Less = x
order _ y _ Equal = y
order _ _ z Greater = z

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
	x < y = order True False False $ x <=> y
	(<=) :: a -> a -> Boolean
	x <= y = order True True False $ x <=> y
	(>) :: a -> a -> Boolean
	x > y = order False False True $ x <=> y
	(>=) :: a -> a -> Boolean
	x >= y = order False True True $ x <=> y
