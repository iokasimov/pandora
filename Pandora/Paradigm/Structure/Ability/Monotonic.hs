module Pandora.Paradigm.Structure.Ability.Monotonic where

class Monotonic e a where
	bypass :: (a -> r -> r) -> r -> e -> r

instance Monotonic a a where
	bypass f r x = f x r
