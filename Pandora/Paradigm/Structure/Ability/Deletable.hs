module Pandora.Paradigm.Structure.Ability.Deletable where

import Pandora.Pattern.Object.Setoid (Setoid)

class Deletable t where
	delete :: Setoid a => a -> t a -> t a
