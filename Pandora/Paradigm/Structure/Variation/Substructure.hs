module Pandora.Paradigm.Structure.These.Substructure (Substructure (..)) where

import Pandora.Paradigm.Inventory.Optics (type (:-.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)

class Substructure f t where
	type Output (f :: * -> k) (t :: * -> *) a
	sub :: t a :-. Tagged f (Output f t a)
