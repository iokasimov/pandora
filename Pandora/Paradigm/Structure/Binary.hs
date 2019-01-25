module Pandora.Paradigm.Structure.Binary (Binary) where

import Pandora.Paradigm.Basis.Junction.Transformer (type (:!:))
import Pandora.Paradigm.Basis.Wye (Wye)
import Pandora.Paradigm.Basis.Cofree (Cofree)

type Binary a = (Cofree Wye :!: Wye) a
