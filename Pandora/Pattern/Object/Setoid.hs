module Pandora.Pattern.Object.Setoid (Boolean (..), (&&), (||), not, bool, Setoid (..)) where

import Pandora.Core.Morphism (($))

infixr ||
infixr 3 &&
infix 4 ==, /=

data Boolean = True | False

(&&) :: Boolean -> Boolean -> Boolean
True && True = True
_ && _ = False

(||) :: Boolean -> Boolean -> Boolean
True || _ = True
_ || True = True
_ || _ = False

not :: Boolean -> Boolean
not True = False
not False = True

bool :: a -> a -> Boolean -> a
bool x _ False = x
bool _ y True = y

{- |
> When providing a new instance, you should ensure it satisfies the four laws:
> * Reflexivity: x == x ≡ True
> * Symmetry: x == y ≡ y == x
> * Transitivity: x == y && y == z ≡ True ===> x == z ≡ True
> * Negation: x /= y ≡ not (x == y)
-}

class Setoid a where
	{-# MINIMAL (==) #-}
	(==) :: a -> a -> Boolean

	(/=) :: a -> a -> Boolean
	(/=) x y = not $ x == y
