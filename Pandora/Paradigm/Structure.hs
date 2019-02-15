module Pandora.Paradigm.Structure (module Exports, Nonempty) where

import Pandora.Paradigm.Structure.Binary as Exports
import Pandora.Paradigm.Structure.Graph as Exports
import Pandora.Paradigm.Structure.Stack as Exports

import Pandora.Paradigm.Basis.Cofree (Cofree)
import Pandora.Paradigm.Junction.Transformer (type (:>:))

-- | Type synonymous for at least one element data structure
type family Nonempty structure a :: * where
	Nonempty (Cofree :>: t) a = Cofree t a
