module Pandora.Pattern.Object.Setoid (Setoid (..)) where

import Pandora.Pattern.Object.Group (invert)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean)

infix 4 ==, /=

{- |
> When providing a new instance, you should ensure it satisfies the four laws:
> * Reflexivity: x == x ≡ True
> * Symmetry: x == y ≡ y == x
> * Transitivity: x == y * y == z ≡ True ===> x == z ≡ True
> * Negation: x /= y ≡ not (x == y)
-}

class Setoid a where
	{-# MINIMAL (==) #-}
	(==) :: a -> a -> Boolean

	(/=) :: a -> a -> Boolean
	(/=) x y = invert (x == y)
