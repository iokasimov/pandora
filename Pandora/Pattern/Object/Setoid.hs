module Pandora.Pattern.Object.Setoid (Setoid (..)) where

import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False, True))

infix 6 ==, !=, ?==

{- |
> When providing a new instance, you should ensure it satisfies:
> * Reflexivity: x == x ≡ True
> * Symmetry: x == y ≡ y == x
> * Transitivity: x == y * y == z ≡ True ===> x == z ≡ True
> * Negation: x != y ≡ invert (x == y)
-}

class Setoid a where
	{-# MINIMAL (==) #-}
	(==) :: a -> a -> Boolean

	(!=) :: a -> a -> Boolean
	(!=) x y = case x == y of
		True -> False
		False -> True

	(?==) :: a -> a -> r -> r -> r
	(?==) x y xc yc = case x == y of
		True -> xc
		False -> yc
