module Pandora.Paradigm.Structure.Ability.Accessible where

import Pandora.Paradigm.Primary.Functor.Exactly (Exactly)
import Pandora.Paradigm.Inventory.Some.Optics (Lens)

class Accessible target source where
	access :: Lens Exactly source target
