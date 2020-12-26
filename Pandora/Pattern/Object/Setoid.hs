module Pandora.Pattern.Object.Setoid (Setoid (..)) where

import Pandora.Pattern.Category (($))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False, True), bool)

infix 4 ==, /=

{- |
> When providing a new instance, you should ensure it satisfies the four laws:
> * Reflexivity: x == x ≡ True
> * Symmetry: x == y ≡ y == x
> * Transitivity: x == y * y == z ≡ True ===> x == z ≡ True
> * Negation: x /= y ≡ invert (x == y)
-}

class Setoid a where
	{-# MINIMAL (==) #-}
	(==) :: a -> a -> Boolean

	(/=) :: a -> a -> Boolean
	-- (/=) x y = (x == y ? False) True
	(/=) x y = bool True False $ x == y
