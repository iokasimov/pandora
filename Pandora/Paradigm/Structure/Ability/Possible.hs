module Pandora.Paradigm.Structure.Ability.Possible where

import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Inventory.Some.Optics (Lens)

class Possible target source where
	perhaps :: Lens Maybe source target
