module Pandora.Paradigm.Structure.Some.Vector where

import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))

data Vector a = forall r . Monotonic a r => Vector r

instance Monotonic a (Vector a) where
	reduce f r (Vector x) = reduce f r x
