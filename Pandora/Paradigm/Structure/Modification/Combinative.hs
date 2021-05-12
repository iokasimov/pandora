module Pandora.Paradigm.Structure.Modification.Combinative where

type family Combinative (s :: * -> *) = (r :: * -> *) | r -> s
