module Pandora.Pattern.Object.Lattice (Lattice) where

import Pandora.Pattern.Object.Semilattice (Infimum, Supremum)

{- |
> When providing a new instance, you should ensure it satisfies:
> * Absorption: a \/ (a /\ b) ≡ a /\ (a \/ b) ≡ a
-}

class (Infimum a, Supremum a) => Lattice a where
