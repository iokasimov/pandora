module Pandora.Paradigm.Structure.Modification.Prefixed where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

newtype Prefixed t k a = Prefixed (t <:.> (Product k <:.> Maybe) := a)

instance Covariant t => Covariant (Prefixed t k) where
	f <$> Prefixed x = Prefixed $ f <$> x

instance Traversable t => Traversable (Prefixed t k) where
	Prefixed x ->> f = Prefixed <$> (x ->> f)

instance (Monoid k, Pointable t) => Pointable (Prefixed t k) where
	point = Prefixed . TU . point . TU . (:*:) zero . Just
