module Pandora.Paradigm.Structure.These.Nonempty (Nonempty) where

-- | Type synonymous for at least one element data structure
type family Nonempty (s :: * -> *) = (r :: * -> *) | r -> s
