module Pandora.Paradigm.Primary.Functor.Predicate (Predicate (..)) where

import Pandora.Core.Morphism ((!))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Determinable (Determinable (determine))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Setoid (Boolean (True))

newtype Predicate a = Predicate { predicate :: a -> Boolean }

instance Contravariant Predicate where
	f >$< g = Predicate $ predicate g . f

instance Determinable Predicate where
	determine = Predicate (True !)
