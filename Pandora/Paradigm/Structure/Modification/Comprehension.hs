{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Modification.Comprehension where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Contravariant ((>$<))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Push), premorph)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))

newtype Comprehension t a = Comprehension (t <:.> Construction t := a)

instance Interpreted (Comprehension t) where
	type Primary (Comprehension t) a = t <:.> Construction t := a
	run ~(Comprehension x) = x
	unite = Comprehension

instance Covariant (t <:.> Construction t) => Covariant (Comprehension t) where
	f <$> Comprehension x = Comprehension $ f <$> x

instance Covariant_ (t <:.> Construction t) (->) (->) => Covariant_ (Comprehension t) (->) (->) where
	f -<$>- Comprehension x = Comprehension $ f -<$>- x

instance (Avoidable t, Pointable t (->)) => Pointable (Comprehension t) (->) where
	point = Comprehension . TU . point . Construct % empty

instance Alternative t => Alternative (Comprehension t) where
	Comprehension x <+> Comprehension y = Comprehension $ x <+> y

instance (Avoidable t, Alternative t) => Avoidable (Comprehension t) where
	empty = Comprehension empty

instance Traversable (t <:.> Construction t) (->) (->) => Traversable (Comprehension t) (->) (->) where
	f <<- Comprehension x = Comprehension -<$>- f <<- x

instance (forall a . Semigroup (t <:.> Construction t := a), Bindable t (->), Pointable t (->), Avoidable t) => Applicative (Comprehension t) where
	fs <*> xs = (\f -> Comprehension . TU . point . point . f =<< xs) =<< fs

instance (forall a . Semigroup (t <:.> Construction t := a), Bindable t (->)) => Bindable (Comprehension t) (->) where
	f =<< Comprehension (TU t) = Comprehension . TU $ (\(Construct x xs) -> run . run $ f x + (f =<< Comprehension (TU xs))) =<< t
		-- t >>= \(Construct x xs) -> run . run $ f x + (Comprehension (TU xs) >>= f)

instance (forall a . Semigroup (t <:.> Construction t := a), Pointable t (->), Avoidable t, Bindable t (->)) => Monad (Comprehension t) where

instance Setoid (t <:.> Construction t := a) => Setoid (Comprehension t a) where
	Comprehension ls == Comprehension rs = ls == rs

instance Semigroup (t <:.> Construction t := a) => Semigroup (Comprehension t a) where
	Comprehension x + Comprehension y = Comprehension $ x + y

instance Monoid (t <:.> Construction t := a) => Monoid (Comprehension t a) where
	zero = Comprehension zero

instance (Covariant t, Covariant_ t (->) (->), Pointable t (->)) => Morphable Push (Comprehension t) where
	type Morphing Push (Comprehension t) = Identity <:.:> Comprehension t := (->)
	morphing (run . premorph -> xs) = T_U $ \(Identity x) -> Comprehension . lift . Construct x . run $ xs

instance Nullable (t <:.> Construction t) => Nullable (Comprehension t) where
	null = run >$< null
