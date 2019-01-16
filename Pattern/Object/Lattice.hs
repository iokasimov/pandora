module Pattern.Object.Lattice (Lattice (..)) where

import Pattern.Object.Semilattice (Infimum, Supremum)

{- |
> When providing a new instance, you should ensure it satisfies the one law:
> * Absorption: a \/ (a /\ b) ≡ a /\ (a \/ b) ≡ a
-}

class (Infimum a, Supremum a) => Lattice a where
