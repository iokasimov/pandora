module Pandora.Paradigm.Structure (module Exports, Nonempty) where

import Pandora.Paradigm.Structure.Binary as Exports
import Pandora.Paradigm.Structure.Graph as Exports
import Pandora.Paradigm.Structure.Stack as Exports

import Pandora.Core.Functor (type (:.:))
import Pandora.Paradigm.Basis.Twister (Twister)

-- | Type synonymous for at least one element data structure
type family Nonempty structure :: * where
	Nonempty ((t :.: Twister t) a) = Twister t a
