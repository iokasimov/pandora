{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Primary.Transformer.Construction where

import Pandora.Core.Functor (type (:.), type (>>>), type (:=>), type (~>))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (<~), (<~~~), (<~~~~)))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), (<------))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--), (<-|------), (<-|-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)), (<-/-/-))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid ((*))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Algebraic ((<-*------), extract)
import Pandora.Paradigm.Algebraic.Exponential (type (<--), type (-->))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>))
import Pandora.Paradigm.Algebraic.Sum ((:+:))
import Pandora.Paradigm.Algebraic.One (One (One))
import Pandora.Paradigm.Algebraic (empty, (<<-|-))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Schemes (TT (TT), T_U (T_U), type (<::>))

data Construction t a = Construct a (t :. Construction t >>> a)

instance Covariant (->) (->) t => Covariant (->) (->) (Construction t) where
	f <-|- ~(Construct x xs) = Construct <------ f x <------ f <-|-|- xs

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => Semimonoidal (-->) (:*:) (:*:) (Construction t) where
	mult = Straight <-- \(Construct x xs :*: Construct y ys) -> Construct <----- x :*: y
		<----- (mult @(-->) <~) <-|-- mult @(-->) <~~~ xs :*: ys

instance (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => Semimonoidal (<--) (:*:) (:*:) (Construction t) where
	mult = Flip <-- \(Construct (x :*: y) xys) -> (Construct x <<-|-) . (Construct y <-|-)
		<----- mult @(<--) <~~~~ (mult @(<--) <~) <-|- xys

instance (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => Monoidal (<--) (-->) (:*:) (:*:) (Construction t) where
	unit _ = Flip <-- \(Construct x _) -> Straight (\_ -> x)

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t, Monoidal (-->) (-->) (:*:) (:+:) t) => Monoidal (-->) (-->) (:*:) (:*:) (Construction t) where
	unit _ = Straight <-- \f -> Construct <-- run f One <-- empty

instance Traversable (->) (->) t => Traversable (->) (->) (Construction t) where
	f <-/- ~(Construct x xs) = Construct <-|------ f x <-*------ f <-/-/- xs

instance Covariant (->) (->) t => Extendable (->) (Construction t) where
	f <<= x = Construct <---- f x <---- (f <<=) <-|- deconstruct x

instance (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => Comonad (->) (Construction t) where

instance (forall u . Semimonoidal (<--) (:*:) (:*:) u) => Lowerable (->) Construction where
	lower x = extract <-|- deconstruct x

instance (forall u . Semimonoidal (<--) (:*:) (:*:) u, forall u . Covariant (->) (->) u) => Hoistable (->) Construction where
	f /|\ x = Construct <---- extract x <---- (/|\) @(->) f <-|- f (deconstruct x)

instance (Setoid a, forall b . Setoid b => Setoid (t b), Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => Setoid (Construction t a) where
	x == y = (extract x == extract y) * (deconstruct x == deconstruct y)

instance (Semigroup a, forall b . Semigroup b => Semigroup (t b), Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => Semigroup (Construction t a) where
	x + y = Construct <-- extract x + extract y <-- deconstruct x + deconstruct y

instance (Monoid a, forall b . Semigroup b => Monoid (t b), Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => Monoid (Construction t a) where
	zero = Construct zero zero

instance Interpreted (->) (Construction t) where
	type Primary (Construction t) a = (Exactly <:*:> t <::> Construction t) a
	run (Construct x xs) = Exactly x <:*:> unite xs
	unite (T_U (Exactly x :*: TT xs)) = Construct x xs

-- instance Monotonic a (t :. Construction t > a) => Monotonic a (Construction t a) where
-- 	reduce f r ~(Construct x xs) = f x <-- reduce f r xs
--
-- instance Monotonic a (t :. Construction t > a) => Monotonic a (t <::> Construction t > a) where
-- 	reduce f r = reduce f r . run

deconstruct :: Construction t a -> t :. Construction t >>> a
deconstruct ~(Construct _ xs) = xs

-- Generate a construction from seed using effectful computation
constitute :: Covariant (->) (->) t => (a -> t a) -> a -> Construction t a
constitute f x = Construct x <---- constitute f <-|- f x

section :: (Comonad (->) t, Monoidal (<--) (-->) (:*:) (:*:) t) => t ~> Construction t
section xs = Construct <--- extract xs <--- section <<= xs
