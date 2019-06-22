module Pandora.Paradigm.Basis.Yoneda (Yoneda (..)) where

import Pandora.Core.Morphism ((.), ($), (!), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (idle))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Adjoint (Adjoint (phi, psi))

newtype Yoneda t a = Yoneda
	{ yoneda :: forall b . (a -> b) -> t b }

instance Covariant (Yoneda t) where
	f <$> x = Yoneda (\k -> yoneda x (k . f))

instance Alternative t => Alternative (Yoneda t) where
	Yoneda f <+> Yoneda g = Yoneda (\k -> f k <+> g k)

instance Applicative t => Applicative (Yoneda t) where
	Yoneda f <*> Yoneda x = Yoneda (\g -> f (g .) <*> x identity)

instance Avoidable t => Avoidable (Yoneda t) where
	idle = Yoneda (idle !)

instance Pointable t => Pointable (Yoneda t) where
	point x = Yoneda (\f -> point $ f x)

instance Extractable t => Extractable (Yoneda t) where
	extract (Yoneda f) = extract $ f identity

instance (Extractable t, Pointable t, Extractable u, Pointable u) => Adjoint (Yoneda t) (Yoneda u) where
	phi f = point . f . point
	psi f = extract . extract . comap f
