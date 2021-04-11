module Pandora.Paradigm.Structure.Some.Vector where

import Pandora.Pattern.Category (($), (/))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Pattern.Object.Group (Group (invert))
import Pandora.Paradigm.Primary.Functor.Product (type (:*:))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))

data Vector r a where
	Scalar :: a -> Vector a a
	Vector :: a -> Vector r a -> Vector (a :*: r) a

instance Semigroup a => Semigroup (Vector a a) where
	Scalar x + Scalar y = Scalar $ x + y

instance (Semigroup a, Semigroup r, Semigroup (a :*: r), Semigroup (Vector r a)) => Semigroup (Vector (a :*: r) a) where
	Vector x xs + Vector y ys = Vector / x + y / xs + ys

instance Ringoid a => Ringoid (Vector a a) where
	Scalar x * Scalar y = Scalar $ x * y

instance (Ringoid a, Ringoid r, Ringoid (a :*: r), Ringoid (Vector r a)) => Ringoid (Vector (a :*: r) a) where
	Vector x xs * Vector y ys = Vector / x * y / xs * ys

instance Monoid a => Monoid (Vector a a) where
	zero = Scalar zero

instance (Monoid a, Monoid r, Monoid (a :*: r), Monoid (Vector r a)) => Monoid (Vector (a :*: r) a) where
	zero = Vector zero zero

instance Quasiring a => Quasiring (Vector a a) where
	one = Scalar one

instance (Quasiring a, Quasiring r, Quasiring (a :*: r), Quasiring (Vector r a)) => Quasiring (Vector (a :*: r) a) where
	one = Vector one one

instance Group a => Group (Vector a a) where
	invert (Scalar x) = Scalar $ invert x

instance (Group a, Group r, Group (a :*: r), Group (Vector r a)) => Group (Vector (a :*: r) a) where
	invert (Vector x xs) = Vector / invert x / invert xs

instance Monotonic a (Vector a a) where
	reduce f r (Scalar x) = f x r

instance Monotonic a (Vector r a) => Monotonic a (Vector (a :*: r) a) where
	reduce f r (Vector x xs) = reduce f / f x r / xs
