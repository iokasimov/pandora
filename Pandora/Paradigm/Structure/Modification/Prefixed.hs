module Pandora.Paradigm.Structure.Modification.Prefixed where

import Pandora.Core.Functor (type (:=))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Primary.Functor.Product (Product)
import Pandora.Paradigm.Schemes.TU (type (<:.>))

newtype Prefixed t k a = Prefixed (t <:.> (Product k <:.> Maybe) := a)
