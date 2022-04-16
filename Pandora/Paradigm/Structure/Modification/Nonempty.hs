module Pandora.Paradigm.Structure.Modification.Nonempty where

-- | Type synonymous for at least one element data structure
type family Nonempty (s :: * -> *) = (r :: * -> *) | r -> s
