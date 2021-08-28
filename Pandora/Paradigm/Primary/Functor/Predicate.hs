module Pandora.Paradigm.Primary.Functor.Predicate where

import Pandora.Core.Functor (type (~>), type (:=>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), bool)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Predicate a = Predicate (a -> Boolean)

instance Interpreted Predicate where
	type Primary Predicate a = a -> Boolean
	run ~(Predicate f) = f
	unite = Predicate

instance Contravariant (->) (->) Predicate where
	f >$< Predicate g = Predicate $ g . f

equate :: Setoid a => a :=> Predicate
equate x = Predicate (== x)

not :: Predicate ~> Predicate
not (Predicate p) = Predicate $ bool True False . p
