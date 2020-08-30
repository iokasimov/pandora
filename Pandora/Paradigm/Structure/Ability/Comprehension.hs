{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Ability.Comprehension where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run))

newtype Comprehension t a = Comprehension (t <:.> Construction t := a)

instance Interpreted (Comprehension t) where
	type Primary (Comprehension t) a = t <:.> Construction t := a
	run (Comprehension x) = x

instance Covariant (t <:.> Construction t) => Covariant (Comprehension t) where
	f <$> Comprehension x = Comprehension $ f <$> x

instance (forall a . Semigroup (t <:.> Construction t := a), Bindable t) => Bindable (Comprehension t) where
	Comprehension (TU t) >>= f = Comprehension . TU $ t >>= \(Construct x xs) -> run $ run (f x) + run (Comprehension (TU xs) >>= f)
