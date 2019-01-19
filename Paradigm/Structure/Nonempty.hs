module Paradigm.Structure.Nonempty (Nonempty) where

import Core.Composition ((:.:))
import Paradigm.Basis.Cofree (Cofree)

type family Nonempty structure :: * where
	Nonempty ((t :.: Cofree t) a) = Cofree t a
