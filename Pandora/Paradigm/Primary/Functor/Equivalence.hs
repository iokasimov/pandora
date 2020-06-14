module Pandora.Paradigm.Primary.Functor.Equivalence (Equivalence (..)) where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant ((.|..))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Paradigm.Primary.Functor.Product (uncurry)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True))

data Equivalence a = Equivalence (a -> a -> Boolean)

instance Contravariant Equivalence where
	f >$< Equivalence g = Equivalence $ \x y -> g (f x) (f y)
