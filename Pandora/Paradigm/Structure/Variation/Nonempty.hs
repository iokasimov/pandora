module Pandora.Paradigm.Structure.Variation.Nonempty (Nonempty) where

-- | Type synonymous for at least one element data structure
type family Nonempty (s :: * -> *) = (r :: * -> *) | r -> s
