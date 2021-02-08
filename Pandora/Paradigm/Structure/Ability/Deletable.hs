module Pandora.Paradigm.Structure.Ability.Deletable where

import Pandora.Pattern.Object.Setoid (Setoid)

infixr 3 -=

class Deletable t where
	(-=) :: Setoid a => a -> t a -> t a
