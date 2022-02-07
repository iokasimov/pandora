module Pandora.Paradigm.Structure.Ability.Monotonic where

import Pandora.Pattern.Category ((<----))
import Pandora.Pattern.Kernel (constant)
import Pandora.Paradigm.Primary.Algebraic.Exponential ((.:..))

class Monotonic a e where
	{-# MINIMAL reduce #-}
	reduce :: (a -> r -> r) -> r -> e -> r

	-- | Version of `reduce` which ignores accumulator
	resolve :: (a -> r) -> r -> e -> r
	resolve g = reduce <---- g .:.. constant

instance Monotonic a a where
	reduce f r x = f x r
