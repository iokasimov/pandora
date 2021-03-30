module Pandora.Paradigm.Structure.Ability.Monotonic where

import Pandora.Pattern ((.|..))
import Pandora.Pattern.Category ((/))
import Pandora.Pattern.Functor ((<+>))
import Pandora.Pattern.Functor.Pointable (Pointable)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Paradigm.Primary.Functor.Function ((!))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate, satisfy)

class Monotonic a e where
	{-# MINIMAL reduce #-}
	reduce :: (a -> r -> r) -> r -> e -> r

	-- | Version of `reduce` which ignores accumulator
	resolve :: (a -> r) -> r -> e -> r
	resolve g = reduce / g .|.. (!)

instance Monotonic a a where
	reduce f r x = f x r

-- Replace it with Morphable instance
-- find :: (Monotonic a e, Pointable t, Avoidable t) => Predicate a -> e -> t a
-- find p struct = reduce (\x r -> r <+> satisfy p x) empty struct
