{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Reverse where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Primary.Transformer.Backwards (Backwards (Backwards))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (point, extract)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Reverse t a = Reverse (t a)

instance Covariant (->) (->) t => Covariant (->) (->) (Reverse t) where
	f <$> Reverse x = Reverse # f <$> x

instance (Semimonoidal (->) (:*:) (:*:) t, Covariant (->) (->) t) => Semimonoidal (->) (:*:) (:*:) (Reverse t) where
	multiply (Reverse x :*: Reverse y) = Reverse # multiply (x :*: y)

instance (Covariant (->) (->) t, Monoidal (->) (->) (:*:) (:*:) t) => Monoidal (->) (->) (:*:) (:*:) (Reverse t) where
	unit _ f = Reverse . point $ f One

instance (Semimonoidal (<--) (:*:) (:*:) t, Covariant (->) (->) t) => Semimonoidal (<--) (:*:) (:*:) (Reverse t) where
	multiply = Flip $ \(Reverse x) -> 
		let Flip f = multiply @(<--) @(:*:) @(:*:) in
		(Reverse <-> Reverse) $ f x

instance (Covariant (->) (->) t, Monoidal (<--) (->) (:*:) (:*:) t) => Monoidal (<--) (->) (:*:) (:*:) (Reverse t) where
	unit _ = Flip $ \(Reverse x) -> (\_ -> extract x)

instance Traversable (->) (->) t => Traversable (->) (->) (Reverse t) where
	f <<- Reverse x = Reverse <$> run (Backwards . f <<- x)

instance Distributive (->) (->) t => Distributive (->) (->) (Reverse t) where
	f -<< x = Reverse $ run . f -<< x

instance Contravariant (->) (->) t => Contravariant (->) (->) (Reverse t) where
	f >$< Reverse x = Reverse # f >$< x

instance Interpreted (Reverse t) where
	type Primary (Reverse t) a = t a
	run ~(Reverse x) = x
	unite = Reverse

instance Liftable (->) Reverse where
	lift = Reverse

instance Lowerable (->) Reverse where
	lower = run

instance Hoistable Reverse where
	f /|\ Reverse x = Reverse # f x
