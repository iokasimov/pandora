module Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty) where

-- | Type synonymous for at least one element data structure
type family Nonempty (s :: * -> *) = (r :: * -> *) | r -> s
