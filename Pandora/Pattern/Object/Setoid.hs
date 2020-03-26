module Pandora.Pattern.Object.Setoid (Boolean (..), (?), not, bool, Setoid (..)) where

import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))

infixr 1 ?
infix 4 ==, /=

data Boolean = True | False

not :: Boolean -> Boolean
not True = False
not False = True

bool :: a -> a -> Boolean -> a
bool x _ False = x
bool _ y True = y

(?) :: Boolean -> a -> a -> a
(?) True x _ = x
(?) False _ y = y

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
	(/=) x y = not (x == y)

instance Setoid Boolean where
	True == True = True
	False == False = True
	_ == _ = False

instance Semigroup Boolean where
	False + False = False
	_ + _ = True

instance Ringoid Boolean where
	True * True = True
	_ * _ = False

instance Monoid Boolean where
	zero = False

instance Quasiring Boolean where
	one = True

instance Group Boolean where
	inverse False = True
	inverse True = False
