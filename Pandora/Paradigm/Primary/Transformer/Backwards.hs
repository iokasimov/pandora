module Pandora.Paradigm.Primary.Transformer.Backwards where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<), (--<<)))
import Pandora.Pattern.Transformation.Liftable (Liftable (lift))
import Pandora.Pattern.Transformation.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformation.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Algebraic ((<-*-))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Exponential (type (--<), type (-->), (%))
import Pandora.Pattern.Operation.One (One (One))
import Pandora.Paradigm.Algebraic (point, extract, (<<-|-))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (<~)))

newtype Backwards t a = Backwards (t a)

instance Covariant (->) (->) t => Covariant (->) (->) (Backwards t) where
	f <-|- Backwards x = Backwards <---- f <-|- x

-- TODO: check that effects evaluation goes in opposite order
instance (Semimonoidal (-->) (:*:) (:*:) t, Covariant (->) (->) t) => Semimonoidal (-->) (:*:) (:*:) (Backwards t) where
	mult = Straight <-- \(Backwards x :*: Backwards y) -> Backwards <---- ((:*:) %) <-|- y <-*- x

instance (Covariant (->) (->) t, Monoidal (-->) (-->) (:*:) (:*:) t) => Monoidal (-->) (-->) (:*:) (:*:) (Backwards t) where
	unit _ = Straight <-- Backwards . point . (<~ One)

instance (Semimonoidal (--<) (:*:) (:*:) t, Covariant (->) (->) t) => Semimonoidal (--<) (:*:) (:*:) (Backwards t) where
	mult = Flip <-- (Backwards <<-|-) . (Backwards <-|-) . (mult @(--<) <~) . run

instance (Covariant (->) (->) t, Monoidal (--<) (-->) (:*:) (:*:) t) => Monoidal (--<) (-->) (:*:) (:*:) (Backwards t) where
	unit _ = Flip <-- \(Backwards x) -> Straight (\_ -> extract x)

instance Traversable (->) (->) t => Traversable (->) (->) (Backwards t) where
	f <-/- Backwards x = Backwards <-|-- f <-/- x

instance Distributive (->) (->) t => Distributive (->) (->) (Backwards t) where
	f -<< x = Backwards <--- run . f -<< x

instance Contravariant (->) (->) t => Contravariant (->) (->) (Backwards t) where
	f >-|- Backwards x = Backwards <---- f >-|- x

instance Interpreted (->) (Backwards t) where
	type Primary (Backwards t) a = t a
	run ~(Backwards x) = x
	unite = Backwards

instance Liftable (->) Backwards where
	lift = Backwards

instance Lowerable (->) Backwards where
	lower = run

instance Hoistable (->) Backwards where
	f /|\ Backwards x = Backwards <-- f x
