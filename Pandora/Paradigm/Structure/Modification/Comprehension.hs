{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Structure.Modification.Comprehension where

import Pandora.Core.Functor (type (>), type (>>>>>), type (>>>>>>))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (<~)))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<), (==<<)))
import Pandora.Pattern.Transformation.Liftable (lift)
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Schemes.TT (TT (TT), type (<::>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Push), premorph)
import Pandora.Pattern.Operation.Exponential (type (-->))
import Pandora.Pattern.Operation.Product ((:*:) ((:*:)))
import Pandora.Pattern.Operation.Sum ((:+:))
import Pandora.Paradigm.Algebraic (empty, (<-|-<-|-))

newtype Comprehension t a = Comprehension (t <::> Construction t >>>>> a)

instance Interpreted (->) (Comprehension t) where
	type Primary (Comprehension t) a = t <::> Construction t >>>>> a
	run ~(Comprehension x) = x
	unite = Comprehension

instance Covariant (->) (->) (t <::> Construction t) => Covariant (->) (->) (Comprehension t) where
	f <-|- Comprehension x = Comprehension <---- f <-|- x

instance Traversable (->) (->) (t <::> Construction t) => Traversable (->) (->) (Comprehension t) where
	f <-/- Comprehension x = Comprehension <-|-- f <-/- x

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) right t, Semimonoidal (-->) (:*:) right (t <::> Construction t)) => Semimonoidal (-->) (:*:) right (Comprehension t) where
	mult = Straight <-- Comprehension . (mult @(-->) @(:*:) @right <~) . ((run :*: run) <-|-<-|-)

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t, Semimonoidal (-->) (:*:) (:*:) (Construction t), Semimonoidal (-->) (:*:) (:+:) t, Semimonoidal (-->) (:*:) (:+:) (Construction t), Monoidal (-->) (-->) (:*:) (:+:) t) => Monoidal (-->) (-->) (:*:) (:+:) (Comprehension t) where
	unit _ = Straight <-- \_ -> Comprehension empty

instance (forall a . Semigroup (t <::> Construction t >>>>> a), Bindable (->) t) => Bindable (->) (Comprehension t) where
	f =<< Comprehension (TT t) = Comprehension . TT <--- (\(Construct x xs) -> run . run @(->) <-- f x + (f ==<< Comprehension <-- TT xs)) =<< t

instance Setoid (t <::> Construction t >>>>> a) => Setoid (Comprehension t a) where
	Comprehension ls == Comprehension rs = ls == rs

instance Semigroup (t <::> Construction t >>>>> a) => Semigroup (Comprehension t a) where
	Comprehension x + Comprehension y = Comprehension <---- x + y

instance Monoid (t <::> Construction t >>>>> a) => Monoid (Comprehension t a) where
	zero = Comprehension zero

instance (Covariant (->) (->) t, Monoidal (-->) (-->) (:*:) (:*:) t) => Morphable Push (Comprehension t) where
	type Morphing Push (Comprehension t) = Exactly <:.:> Comprehension t >>>>>> (->)
	morphing (run . premorph -> xs) = T_U <-- \(Exactly x) -> Comprehension . lift . Construct x . run <-- xs
