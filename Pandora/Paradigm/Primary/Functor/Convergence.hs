module Pandora.Paradigm.Primary.Functor.Convergence where

import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Paradigm.Primary.Algebraic ()

data Convergence r a = Convergence (a -> a -> r)

instance Contravariant (->) (->) (Convergence r) where
	f >$< Convergence g = Convergence $ \x y -> g # f x # f y
