module Pandora.Paradigm.Structure.Modification.Nonempty where

import Pandora.Paradigm.Schemes (type (<::>))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Primary.Transformer.Construction (Construction)

-- | At least one element data structure
type family Nonempty (structure :: * -> *) where
	Nonempty (Maybe <::> Construction t) = Construction t
