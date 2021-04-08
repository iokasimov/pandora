module Pandora.Paradigm.Structure.Some.Vector where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))

newtype Vector r a = Vector r

instance Monotonic a r => Monotonic a (Vector r a) where
	reduce f r (Vector x) = reduce f r x

instance Semigroup r => Semigroup (Vector r a) where
	Vector x + Vector y = Vector $ x + y

instance Ringoid r => Ringoid (Vector r a) where
	Vector x * Vector y = Vector $ x * y

instance Monoid r => Monoid (Vector r a) where
	zero = Vector zero

instance Quasiring r => Quasiring (Vector r a) where
	one = Vector one
