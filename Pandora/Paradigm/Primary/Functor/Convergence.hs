module Pandora.Paradigm.Primary.Functor.Convergence where

import Pandora.Pattern.Category (($), (/))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Divisible (Divisible ((>*<)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))

data Convergence r a = Convergence (a -> a -> r)

instance Contravariant (Convergence r) where
	f >$< Convergence g = Convergence $ \x y -> g / f x / f y

instance Ringoid r => Divisible (Convergence r) where
	Convergence g >*< Convergence h = Convergence $
		\(x :*: x') (y :*: y') -> g x y * h x' y'
