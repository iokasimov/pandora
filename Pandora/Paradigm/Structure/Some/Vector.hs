module Pandora.Paradigm.Structure.Some.Vector where

import Pandora.Pattern.Category (($), (/))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Functor.Product (type (:*:))

data Vector r a where
	Scalar :: a -> Vector a a
	Vector :: a -> Vector r a -> Vector (a :*: r) a

instance Semigroup a => Semigroup (Vector a a) where
	Scalar x + Scalar y = Scalar $ x + y

instance (Semigroup a, Semigroup r, Semigroup (a :*: r), Semigroup (Vector r a)) => Semigroup (Vector (a :*: r) a) where
	Vector x xs + Vector y ys = Vector / x + y / xs + ys
