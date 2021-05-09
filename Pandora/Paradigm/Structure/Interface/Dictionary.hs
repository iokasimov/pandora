module Pandora.Paradigm.Structure.Interface.Dictionary where

import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Lookup))

type Dictionary f t = Morphable (Lookup f) t
