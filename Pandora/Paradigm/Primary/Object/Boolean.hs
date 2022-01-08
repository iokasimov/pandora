module Pandora.Paradigm.Primary.Object.Boolean where

import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))

data Boolean = True | False

bool :: a -> a -> Boolean -> a
bool x _ False = x
bool _ y True = y

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
