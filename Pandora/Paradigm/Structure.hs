module Pandora.Paradigm.Structure (Nonempty, module Exports) where

import Pandora.Paradigm.Structure.Binary as Exports
import Pandora.Paradigm.Structure.Graph as Exports
import Pandora.Paradigm.Structure.Stack as Exports

import Pandora.Paradigm.Basis.Cofree (Cofree)
import Pandora.Paradigm.Basis.Junction.Transformer (type (:!:))

type family Nonempty structure :: * where
	Nonempty ((Cofree t :!: t) a) = Cofree t a
