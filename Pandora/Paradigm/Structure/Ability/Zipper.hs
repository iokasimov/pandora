module Pandora.Paradigm.Structure.Ability.Zipper where

type family Zipper (s :: * -> *) = (r :: * -> *) | r -> s
