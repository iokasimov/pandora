module Pandora.Paradigm.Structure (module Exports, Nonempty) where

import Pandora.Paradigm.Structure.Specific.Binary as Exports
import Pandora.Paradigm.Structure.Specific.Graph as Exports
import Pandora.Paradigm.Structure.Specific.Stack as Exports

import Pandora.Paradigm.Basis.Maybe (Maybe)
import Pandora.Paradigm.Basis.Twister (Twister)

-- | Type synonymous for at least one element data structure
type family Nonempty (structure :: * -> *) where
	Nonempty Stack = Twister Maybe
