module Pandora.Paradigm.Structure.Binary (Binary) where

import Pandora.Core.Functor (type (:.:))
import Pandora.Paradigm.Basis.Wye (Wye)
import Pandora.Paradigm.Basis.Cofree (Cofree)

type Binary a = (Wye :.: Cofree Wye) a
