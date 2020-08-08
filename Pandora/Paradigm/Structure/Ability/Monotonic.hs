module Pandora.Paradigm.Structure.Ability.Monotonic where

class Monotonic e a where
	iterate :: (a -> r -> r) -> r -> e -> r

instance Monotonic a a where
	iterate f r x = f x r
