module Pandora.Paradigm.Primary.Functor.Predicate where

import Pandora.Core.Functor (type (~>), type (:=>))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), bool)
import Pandora.Paradigm.Algebraic.Product ((:*:)((:*:)))
import Pandora.Paradigm.Algebraic.Sum ((:+:)(Option, Adoption))
import Pandora.Paradigm.Algebraic.Exponential (type (-->), type (<--))

newtype Predicate a = Predicate (a -> Boolean)

instance Interpreted (->) Predicate where
	type Primary Predicate a = a -> Boolean
	run ~(Predicate f) = f
	unite = Predicate

instance Contravariant (->) (->) Predicate where
	f >-|- Predicate g = Predicate <-- g . f

instance Semimonoidal (-->) (:*:) (:*:) Predicate where
	mult = Straight <-- \(Predicate f :*: Predicate g) -> Predicate <-- \(x :*: y) -> f x * g y

instance Monoidal (-->) (<--) (:*:) (:*:) Predicate where
	unit _ = Straight <-- \_ -> Predicate <-- \_ -> True

instance Semimonoidal (-->) (:*:) (:+:) Predicate where
	mult = Straight <-- \(Predicate f :*: Predicate g) -> Predicate <-- \case
		Option x -> f x
		Adoption y -> g y

equate :: Setoid a => a :=> Predicate
equate x = Predicate (== x)

not :: Predicate ~> Predicate
not (Predicate p) = Predicate <-- bool True False . p
