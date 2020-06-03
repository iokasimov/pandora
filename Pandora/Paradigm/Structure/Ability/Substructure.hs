module Pandora.Paradigm.Structure.Ability.Substructure where

import Pandora.Paradigm.Inventory.Optics (type (:-.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)

class Substructure f t where
	type Substructural (f :: * -> k) (t :: * -> *) a
	sub :: Tagged f (t a) :-. Substructural f t a
