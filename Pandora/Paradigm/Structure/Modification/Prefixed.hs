{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Modification.Prefixed where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant ((>$<))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Setoid (Setoid)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), attached)
import Pandora.Paradigm.Primary.Functor.Predicate (equate)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic, find)
import Pandora.Paradigm.Structure.Interface.Dictionary (Dictionary ((?=)))

type Keyed k = Product k <:.> Maybe

newtype Prefixed t k a = Prefixed (t <:.> Keyed k := a)

instance Interpreted (Prefixed t k) where
	type Primary (Prefixed t k) a = t <:.> Keyed k := a
	run ~(Prefixed x) = x
	unite = Prefixed

instance Covariant t => Covariant (Prefixed t k) where
	f <$> Prefixed x = Prefixed $ f <$> x

instance Traversable t => Traversable (Prefixed t k) where
	Prefixed x ->> f = Prefixed <$> (x ->> f)

instance (Monoid k, Pointable t) => Pointable (Prefixed t k) where
	point = Prefixed . lift . TU . (:*:) zero . Just

instance (Monotonic (Keyed k a) (t (Keyed k a)), Setoid k) => Dictionary a k (Prefixed t k) where
	k ?= Prefixed x = find @(Keyed k a) (attached . run >$< equate k) (run x) >>= extract . run
