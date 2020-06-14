module Pandora.Paradigm.Primary.Functor.Predicate (Predicate (..)) where

import Pandora.Core.Morphism ((!))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Determinable (Determinable (determine))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True))

newtype Predicate a = Predicate (a -> Boolean)

instance Contravariant Predicate where
	f >$< Predicate g = Predicate $ g . f

instance Determinable Predicate where
	determine = Predicate (True !)
