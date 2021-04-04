module Pandora.Pattern.Object.Semiring (Semiring) where

import Pandora.Pattern.Object.Ringoid (Ringoid)

{- |
> When providing a new instance, you should ensure it satisfies:
> * Associativity: x * (y * z) ≡ (x * y) * z
-}

class Ringoid a => Semiring a where
