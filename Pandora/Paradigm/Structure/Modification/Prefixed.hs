{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Modification.Prefixed where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), attached)
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic, find)
import Pandora.Paradigm.Structure.Interface.Dictionary (Dictionary ((?=)))

type Keyed k = Product k <:.> Maybe

newtype Prefixed t k a = Prefixed (t <:.> Keyed k := a)

instance Covariant t => Covariant (Prefixed t k) where
	f <$> Prefixed x = Prefixed $ f <$> x

instance Traversable t => Traversable (Prefixed t k) where
	Prefixed x ->> f = Prefixed <$> (x ->> f)

instance (Monoid k, Pointable t) => Pointable (Prefixed t k) where
	point = Prefixed . TU . point . TU . (:*:) zero . Just

instance (Monotonic (Keyed k a) (t (Keyed k a)), Setoid k) => Dictionary a k (Prefixed t k) where
	k ?= Prefixed x = find @(Keyed k a) @(t (Keyed k a)) (Predicate $ (== k) . attached . run) (run x) >>= extract . run
