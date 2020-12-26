module Pandora.Paradigm.Primary.Object.Boolean where

import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))

infixr 1 ?

data Boolean = True | False

bool :: a -> a -> Boolean -> a
bool x _ False = x
bool _ y True = y

(?) :: Boolean -> a -> a -> a
(?) True x _ = x
(?) False _ y = y

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
