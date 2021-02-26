module Pandora.Paradigm.Primary.Functor.Equivalence where

import Pandora.Pattern.Category (($), (/))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Divisible (Divisible ((>*<)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))

data Equivalence a = Equivalence (a -> a -> Boolean)

instance Contravariant Equivalence where
	f >$< Equivalence g = Equivalence $ \x y -> g / f x / f y

instance Divisible Equivalence where
	Equivalence g >*< Equivalence h = Equivalence $
		\(x :*: x') (y :*: y') -> g x y * h x' y'
