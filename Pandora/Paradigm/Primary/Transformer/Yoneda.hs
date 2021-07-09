{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Yoneda where

import Pandora.Pattern.Category (identity, (.), ($), (#))
import Pandora.Pattern.Functor ((<*+>))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Paradigm.Primary.Functor.Function ((!.))

newtype Yoneda t a = Yoneda
	{ yoneda :: forall b . (a -> b) -> t b }

instance Covariant (Yoneda t) where
	f <$> x = Yoneda (\k -> yoneda x (k . f))

instance Covariant_ (Yoneda t) (->) (->) where
	f -<$>- x = Yoneda (\k -> yoneda x (k . f))

instance Alternative t => Alternative (Yoneda t) where
	Yoneda f <+> Yoneda g = Yoneda (f <*+> g)

instance Applicative t => Applicative (Yoneda t) where
	Yoneda f <*> Yoneda x = Yoneda (\g -> f (g .) <*> x identity)

instance Avoidable t => Avoidable (Yoneda t) where
	empty = Yoneda (empty !.)

instance Pointable t (->) => Pointable (Yoneda t) (->) where
	point x = Yoneda (\f -> point $ f x)

instance Extractable t (->) => Extractable (Yoneda t) (->) where
	extract (Yoneda f) = extract $ f identity

instance Liftable Yoneda where
	lift x = Yoneda (-<$>- x)

instance (Extractable t (->), Pointable t (->), Extractable u (->) , Pointable u (->)) => Adjoint (Yoneda t) (Yoneda u) where
	x -| f = point . f . point # x
	x |- g = extract . extract # g <$> x
