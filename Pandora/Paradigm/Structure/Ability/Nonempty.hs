module Pandora.Paradigm.Structure.Ability.Nonempty where

-- | Type synonymous for at least one element data structure
type family Nonempty (s :: * -> *) = (r :: * -> *) | r -> s
