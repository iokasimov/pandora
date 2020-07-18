module Pandora.Paradigm.Structure.Ability.Monotonic where

import Pandora.Core.Functor (type (|->))

class Monotonic a i where
	iterate :: a |-> t -> i -> t a
