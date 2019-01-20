module Pandora.Paradigm.Structure (Nonempty, module Exports) where

import Pandora.Paradigm.Structure.Graph as Exports
import Pandora.Paradigm.Structure.Stack as Exports

import Pandora.Core.Composition ((:.:))
import Pandora.Paradigm.Basis.Cofree (Cofree)

type family Nonempty structure :: * where
	Nonempty ((t :.: Cofree t) a) = Cofree t a
