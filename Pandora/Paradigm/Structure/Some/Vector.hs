module Pandora.Paradigm.Structure.Some.Vector where

import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic)

data Vector a = forall v . Monotonic a v => Vector v
