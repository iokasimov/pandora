module Pandora.Paradigm.Structure.Nonempty (Nonempty) where

import Pandora.Paradigm.Basis.Maybe (Maybe)
import Pandora.Paradigm.Basis.Edges (Edges)
import Pandora.Paradigm.Basis.Twister (Twister)
import Pandora.Paradigm.Basis.Wye (Wye)
import Pandora.Paradigm.Structure.Specific.Binary (Binary)
import Pandora.Paradigm.Structure.Specific.Graph (Graph)
import Pandora.Paradigm.Structure.Specific.Stack (Stack)

-- | Type synonymous for at least one element data structure
type family Nonempty (structure :: * -> *) where
	Nonempty Stack = Twister Maybe
	Nonempty Graph = Twister Edges
	Nonempty Binary = Twister Wye
