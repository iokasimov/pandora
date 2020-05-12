module Pandora.Paradigm.Primary.Object.Boolean (Boolean (..), bool, (?)) where

import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Pattern.Object.Group (Group (invert))

infixr 1 ?

data Boolean = True | False

bool :: a -> a -> Boolean -> a
bool x _ False = x
bool _ y True = y

(?) :: Boolean -> a -> a -> a
(?) True x _ = x
(?) False _ y = y

-- instance Setoid Boolean where
-- 	True == True = True
-- 	False == False = True
-- 	_ == _ = False

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
	invert False = True
	invert True = False
