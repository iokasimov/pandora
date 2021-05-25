module Pandora.Paradigm.Structure.Ability.Zipper where

import Pandora.Core.Functor (type (:::))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Rotate))

type family Zipper (structure :: * -> *) (moves :: k) = (result :: * -> *) | result -> structure

type family Fastenable structure rs where
	Fastenable structure (r ::: rs) = (Morphable (Rotate r) structure, Fastenable structure rs)
	Fastenable structure r = Morphable (Rotate r) structure
