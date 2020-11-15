module Pandora.Paradigm.Structure.Ability.Monotonic where

import Pandora.Pattern.Functor ((<+>))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate, satisfy)

class Monotonic e a where
	bypass :: (a -> r -> r) -> r -> e -> r

instance Monotonic a a where
	bypass f r x = f x r

find :: Monotonic e a => Predicate a -> e -> Maybe a
find p struct = bypass (\x r -> r <+> satisfy p x) Nothing struct
