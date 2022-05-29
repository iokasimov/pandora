module Pandora.Paradigm.Structure.Ability.Monotonic where

import Pandora.Pattern.Category ((<----))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Operation.Exponential ((.:..))
import Pandora.Pattern.Operation.Sum ((:+:) (Option, Adoption))

class Monotonic a e where
	{-# MINIMAL reduce #-}
	reduce :: (a -> r -> r) -> r -> e -> r

	-- | Version of `reduce` which ignores accumulator
	resolve :: (a -> r) -> r -> e -> r
	resolve g = reduce <---- g .:.. constant

instance Monotonic a a where
	reduce f r x = f x r

instance Monotonic a (o :+: a) where
	reduce fun def (Adoption x) = fun x def
	reduce _ def (Option _) = def

instance Monotonic o (o :+: a) where
	reduce fun def (Option x) = fun x def
	reduce _ def (Adoption _) = def
