module Pandora.Paradigm.Primary.Functor.Convergence where

import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor (Functor ((-|-)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Operation.Exponential (type (-->), type (--<))
import Pandora.Paradigm.Algebraic.Product ((:*:)((:*:)))
import Pandora.Paradigm.Algebraic ()

data Convergence r a = Convergence (a -> a -> r)

instance Functor (-->) (--<) (Convergence r) where
	(-|-) (Straight f) = Flip <-- \case
		Convergence g -> Convergence <-- \x y -> g <-- f x <-- f y

instance Contravariant (->) (->) (Convergence r) where
	f >-|- Convergence g = Convergence <-- \x y -> g <-- f x <-- f y

instance Semigroup r => Semimonoidal (-->) (:*:) (:*:) (Convergence r) where
	mult = Straight <-- \(Convergence f :*: Convergence g) -> Convergence <-- \(a :*: b) (a' :*: b') -> f a a' + g b b'

instance Monoid r => Monoidal (-->) (--<) (:*:) (:*:) (Convergence r) where
	unit _ = Straight <-- \_ -> Convergence <-- \_ _ -> zero
