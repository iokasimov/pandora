module Pandora.Paradigm.Structure.Variation.Substructure (Substructure (..)) where

import Pandora.Paradigm.Inventory.Optics (type (:-.))

class Substructure f t where
	type Output (f :: * -> k) (t :: * -> *) a = r | r -> f t a
	sub :: t a :-. Output f t a
