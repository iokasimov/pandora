{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Linear.Matrix where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Linear.Vector (Vector)

newtype Matrix i j a = Matrix (Vector i (Vector j a))

instance (Semigroup a, Semigroup (Vector i a), Semigroup (Vector i (Vector i a))) => Semigroup (Matrix i i a) where
	~(Matrix x) + ~(Matrix y) = Matrix $ x + y
