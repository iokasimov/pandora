module Pandora.Paradigm.Structure.Ability.Accessible where

import Pandora.Paradigm.Inventory.Optics (type (:-.))

class Accessible tgt src where
	access :: src :-. tgt
