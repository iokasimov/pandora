module Pandora.Paradigm.Structure.Ability.Accessible where

import Pandora.Paradigm.Inventory.Optics (type (:-.))

class Accessible a e where
	access :: a :-. e
