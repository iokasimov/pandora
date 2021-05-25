module Pandora.Paradigm.Structure.Ability.Zipper where

type family Zipper (structure :: * -> *) (moves :: k) = (result :: * -> *) | result -> structure
