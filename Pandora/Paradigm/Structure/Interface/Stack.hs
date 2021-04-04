module Pandora.Paradigm.Structure.Interface.Stack where

import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Push, Pop))

{- |
> When providing a new instance, you should ensure it satisfies:
> * Idempotency: item @Push x . morph @Pop ≡ identity
-}

class (Morphable Push t, Morphable Pop t) => Stack t where
