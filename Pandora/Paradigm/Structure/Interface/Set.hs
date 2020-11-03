module Pandora.Paradigm.Structure.Interface.Set where

import Pandora.Pattern.Object.Setoid (Setoid)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean)

class Set t where
	member :: Setoid a => a -> t a -> Boolean
	subset :: Setoid a => t a -> t a -> Boolean
