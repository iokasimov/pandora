module Pandora.Paradigm.Primary.Transformer.Backwards where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Primary.Algebraic ((-<*>-))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--), (%))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (point, extract)
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Backwards t a = Backwards (t a)

instance Covariant (->) (->) t => Covariant (->) (->) (Backwards t) where
	f <$> Backwards x = Backwards $ f <$> x

-- TODO: check that effects evaluation goes in opposite order
instance (Semimonoidal (->) (:*:) (:*:) t, Covariant (->) (->) t) => Semimonoidal (->) (:*:) (:*:) (Backwards t) where
	mult (Backwards x :*: Backwards y) = Backwards #
		((:*:) %) <$> y -<*>- x

instance (Covariant (->) (->) t, Monoidal (->) (->) (:*:) (:*:) t) => Monoidal (->) (->) (:*:) (:*:) (Backwards t) where
	unit _ f = Backwards . point $ f One

instance (Semimonoidal (<--) (:*:) (:*:) t, Covariant (->) (->) t) => Semimonoidal (<--) (:*:) (:*:) (Backwards t) where
	mult = Flip $ (Backwards <-> Backwards) . run (mult @(<--) @(:*:) @(:*:)) . run

instance (Covariant (->) (->) t, Monoidal (<--) (->) (:*:) (:*:) t) => Monoidal (<--) (->) (:*:) (:*:) (Backwards t) where
	unit _ = Flip $ \(Backwards x) -> (\_ -> extract x)

instance Traversable (->) (->) t => Traversable (->) (->) (Backwards t) where
	f <<- Backwards x = Backwards <$> f <<- x

instance Distributive (->) (->) t => Distributive (->) (->) (Backwards t) where
	f -<< x = Backwards $ run . f -<< x

instance Contravariant (->) (->) t => Contravariant (->) (->) (Backwards t) where
	f >$< Backwards x = Backwards $ f >$< x

instance Interpreted (->) (Backwards t) where
	type Primary (Backwards t) a = t a
	run ~(Backwards x) = x
	unite = Backwards

instance Liftable (->) Backwards where
	lift = Backwards

instance Lowerable (->) Backwards where
	lower = run

instance Hoistable Backwards where
	f /|\ Backwards x = Backwards $ f x
