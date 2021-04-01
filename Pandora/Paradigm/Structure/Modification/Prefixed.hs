{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Modification.Prefixed where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Prefixed t k a = Prefixed (t :. Product k := a)

instance Interpreted (Prefixed t k) where
	type Primary (Prefixed t k) a = t :. Product k := a
	run ~(Prefixed x) = x
	unite = Prefixed

instance Covariant t => Covariant (Prefixed t k) where
	f <$> Prefixed x = Prefixed $ f <$$> x

instance Traversable t => Traversable (Prefixed t k) where
	Prefixed x ->> f = Prefixed <$> x ->>> f

instance (Monoid k, Pointable t) => Pointable (Prefixed t k) where
	point = Prefixed . point . (:*:) zero
