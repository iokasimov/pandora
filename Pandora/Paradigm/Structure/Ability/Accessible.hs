module Pandora.Paradigm.Structure.Ability.Accessible where

import Pandora.Paradigm.Primary.Functor.Identity (Identity)
import Pandora.Paradigm.Inventory.Some.Optics (Lens)

class Accessible target source where
	access :: Lens Identity source target
