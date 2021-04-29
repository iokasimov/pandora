module Pandora.Paradigm.Structure.Ability.Monotonic where

import Pandora.Pattern.Category ((#))
import Pandora.Pattern.Functor.Covariant ((.#..))
import Pandora.Paradigm.Primary.Functor.Function ((!))

class Monotonic a e where
	{-# MINIMAL reduce #-}
	reduce :: (a -> r -> r) -> r -> e -> r

	-- | Version of `reduce` which ignores accumulator
	resolve :: (a -> r) -> r -> e -> r
	resolve g = reduce # g .#.. (!)

instance Monotonic a a where
	reduce f r x = f x r
