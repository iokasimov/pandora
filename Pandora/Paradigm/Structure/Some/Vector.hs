module Pandora.Paradigm.Structure.Some.Vector where

import Pandora.Paradigm.Primary.Functor.Product (type (:*:))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic)

data Vector a = forall r . (Monotonic a (a :*: r), Monotonic a r) => Vector (a :*: r)
