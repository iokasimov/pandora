module Pandora.Paradigm.Primary.Functor.Predicate where

import Pandora.Core.Functor (type (~>), type (|->))
import Pandora.Core.Morphism ((!))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Divisible (Divisible ((>*<)))
import Pandora.Pattern.Functor.Determinable (Determinable (determine))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), bool, (?))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))

newtype Predicate a = Predicate (a -> Boolean)

instance Contravariant Predicate where
	f >$< Predicate g = Predicate $ g . f

instance Divisible Predicate where
	Predicate g >*< Predicate h = Predicate $ \(b :*: c) -> g b * h c

instance Determinable Predicate where
	determine = Predicate (True !)

equate :: Setoid a => a |-> Predicate
equate x = Predicate (== x)

satisfy :: (Pointable t, Avoidable t) => Predicate a -> a -> t a
satisfy (Predicate p) x = p x ? point x $ empty

not :: Predicate ~> Predicate
not (Predicate p) = Predicate $ bool True False . p
