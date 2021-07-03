module Pandora.Paradigm.Primary.Functor.Predicate where

import Pandora.Core.Functor (type (~>), type (:=>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)), Contravariant_ ((->$<-)))
import Pandora.Pattern.Functor.Divisible (Divisible ((>*<)), Divisible' (divide))
import Pandora.Pattern.Functor.Determinable (Determinable (determine))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Ringoid ((*))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), bool, (?))
import Pandora.Paradigm.Primary.Functor.Function ((!.))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Predicate a = Predicate (a -> Boolean)

instance Interpreted Predicate where
	type Primary Predicate a = a -> Boolean
	run ~(Predicate f) = f
	unite = Predicate

instance Contravariant Predicate where
	f >$< Predicate g = Predicate $ g . f

instance Contravariant_ Predicate (->) (->) where
	f ->$<- Predicate g = Predicate $ g . f

instance Divisible Predicate where
	Predicate g >*< Predicate h = Predicate $ \(b :*: c) -> g b * h c

instance Divisible' Predicate (:*:) where
	divide f (Predicate g :*: Predicate h) = Predicate $ \r -> case f r of
		b :*: c -> g b * h c

instance Determinable Predicate where
	determine = Predicate (True !.)

equate :: Setoid a => a :=> Predicate
equate x = Predicate (== x)

satisfy :: (Pointable t (->), Avoidable t) => Predicate a -> a -> t a
satisfy p x = run p x ? point x $ empty

not :: Predicate ~> Predicate
not (Predicate p) = Predicate $ bool True False . p
