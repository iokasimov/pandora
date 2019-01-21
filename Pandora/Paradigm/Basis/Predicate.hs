module Pandora.Paradigm.Basis.Predicate (Predicate (..)) where

import Pandora.Core.Morphism ((.), ($))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Object.Setoid (Boolean)

newtype Predicate a = Predicate { predicate :: a -> Boolean }

instance Contravariant Predicate where
	f >$< g = Predicate $ predicate g . f
