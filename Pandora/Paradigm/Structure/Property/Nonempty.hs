module Pandora.Paradigm.Structure.Property.Nonempty (Nonempty) where

import Pandora.Paradigm.Basis.Cofree (Cofree)
import Pandora.Paradigm.Junction.Transformer (type (:>:))

-- | Type synonymous for at least one element data structure
type family Nonempty structure a :: * where
	Nonempty (Cofree :>: t) a = Cofree t a
