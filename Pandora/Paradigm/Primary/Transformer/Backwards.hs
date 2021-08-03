module Pandora.Paradigm.Primary.Transformer.Backwards where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply_))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Primary.Algebraic ((-<*>-))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Backwards t a = Backwards (t a)

instance Covariant_ t (->) (->) => Covariant_ (Backwards t) (->) (->) where
	f -<$>- Backwards x = Backwards $ f -<$>- x

instance Pointable t (->) => Pointable (Backwards t) (->) where
	point = Backwards . point

instance Extractable t (->) => Extractable (Backwards t) (->) where
	extract (Backwards x) = extract x

-- TODO: check that effects evaluation goes in opposite order
instance (Semimonoidal t (->) (:*:) (:*:), Covariant_ t (->) (->)) => Semimonoidal (Backwards t) (->) (:*:) (:*:) where
	multiply_ (Backwards x :*: Backwards y) = Backwards #
		((:*:) %) -<$>- y -<*>- x

instance Traversable t (->) (->) => Traversable (Backwards t) (->) (->) where
	f <<- Backwards x = Backwards -<$>- f <<- x

instance Distributive t (->) (->) => Distributive (Backwards t) (->) (->) where
	f -<< x = Backwards $ run . f -<< x

instance Contravariant t => Contravariant (Backwards t) where
	f >$< Backwards x = Backwards $ f >$< x

instance Interpreted (Backwards t) where
	type Primary (Backwards t) a = t a
	run ~(Backwards x) = x
	unite = Backwards

instance Liftable Backwards where
	lift = Backwards

instance Lowerable Backwards where
	lower = run

instance Hoistable Backwards where
	f /|\ Backwards x = Backwards $ f x
