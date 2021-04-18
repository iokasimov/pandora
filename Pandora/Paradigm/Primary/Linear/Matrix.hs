{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Linear.Matrix where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Primary.Linear.Vector (Vector)

newtype Matrix i j a = Matrix (Vector i (Vector j a))

instance (Semigroup a, Semigroup (Vector i a), Semigroup (Vector i (Vector j a))) => Semigroup (Matrix i j a) where
	~(Matrix x) + ~(Matrix y) = Matrix $ x + y

instance (Monoid a, Monoid (Vector i a), Monoid (Vector i (Vector j a))) => Monoid (Matrix i j a) where
	zero = Matrix zero

instance (Setoid a, Setoid (Vector i a), Setoid (Vector i (Vector j a))) => Setoid (Matrix i j a) where
	Matrix x == Matrix y = x == y
