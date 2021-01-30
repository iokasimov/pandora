module Pandora.Paradigm.Structure.Ability.Deletable where

import Pandora.Paradigm.Primary.Functor.Predicate (Predicate)

class Deletable t where
	delete :: Predicate a -> t a -> t a
