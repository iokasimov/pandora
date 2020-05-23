module Pandora.Paradigm.Structure.Ability.Zipper (Zipper) where

type family Zipper (s :: * -> *) = (r :: * -> *) | r -> s
