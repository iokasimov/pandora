module Pandora.Paradigm.Structure.Some.Vector where

import Pandora.Paradigm.Primary.Functor.Product (type (:*:))

data Vector r a where
	Scalar :: a -> Vector a a
	Vector :: a -> Vector r a -> Vector (a :*: r) a
