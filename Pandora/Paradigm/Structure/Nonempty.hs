module Pandora.Paradigm.Structure.Nonempty (Nonempty) where

import Pandora.Paradigm.Basis.Maybe (Maybe)
import Pandora.Paradigm.Basis.Twister (Twister)
import Pandora.Paradigm.Structure.Specific.Stack (Stack)

-- | Type synonymous for at least one element data structure
type family Nonempty (structure :: * -> *) where
	Nonempty Stack = Twister Maybe
