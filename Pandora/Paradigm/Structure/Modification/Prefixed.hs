module Pandora.Paradigm.Structure.Modification.Prefixed where

import Pandora.Paradigm.Schemes (type (<::>))
import Pandora.Paradigm.Algebraic.Product ((:*:))

type Prefixed t k = t <::> (:*:) k
