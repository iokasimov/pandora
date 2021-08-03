{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Yoneda where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()

newtype Yoneda t a = Yoneda
	{ yoneda :: forall b . (a -> b) -> t b }

instance Covariant_ (Yoneda t) (->) (->) where
	f -<$>- x = Yoneda (\k -> yoneda x (k . f))

instance Pointable t (->) => Pointable (Yoneda t) (->) where
	point x = Yoneda (\f -> point $ f x)

instance Extractable t (->) => Extractable (Yoneda t) (->) where
	extract (Yoneda f) = extract $ f identity

instance Liftable Yoneda where
	lift x = Yoneda (-<$>- x)

instance (Extractable t (->), Pointable t (->), Extractable u (->) , Pointable u (->)) => Adjoint (Yoneda t) (Yoneda u) (->) (->) where
	f -| x = point . f . point # x
	g |- x = extract . extract # g -<$>- x
