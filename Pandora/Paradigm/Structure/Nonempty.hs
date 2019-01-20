module Pandora.Paradigm.Structure.Nonempty (Nonempty) where

import Pandora.Core.Composition ((:.:))
import Pandora.Paradigm.Basis.Cofree (Cofree)

type family Nonempty structure :: * where
	Nonempty ((t :.: Cofree t) a) = Cofree t a
