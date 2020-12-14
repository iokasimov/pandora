module Pandora.Paradigm.Structure.Ability.Monotonic where

import Pandora.Pattern.Functor ((<+>))
import Pandora.Pattern.Functor.Pointable (Pointable)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate, satisfy)

class Monotonic e a where
	reduce :: (a -> r -> r) -> r -> e -> r

instance Monotonic a a where
	reduce f r x = f x r

find :: (Monotonic e a, Pointable t, Avoidable t) => Predicate a -> e -> t a
find p struct = reduce (\x r -> r <+> satisfy p x) empty struct
