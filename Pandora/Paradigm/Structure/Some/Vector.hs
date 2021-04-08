module Pandora.Paradigm.Structure.Some.Vector where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))

newtype Vector r a = Vector r

instance Monotonic a r => Monotonic a (Vector r a) where
	reduce f r (Vector x) = reduce f r x

instance Semigroup r => Semigroup (Vector r a) where
	Vector x + Vector y = Vector $ x + y
