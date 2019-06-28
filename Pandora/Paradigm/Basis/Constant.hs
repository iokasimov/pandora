module Pandora.Paradigm.Basis.Constant (Constant (..)) where

import Pandora.Core.Morphism (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Invariant (Invariant (invmap))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (unit))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (inverse))

newtype Constant a b = Constant a

instance Covariant (Constant a) where
	_ <$> Constant x = Constant x

instance Contravariant (Constant a) where
	_ >$< Constant x = Constant x

instance Invariant (Constant a) where
	invmap _ _ (Constant x) = Constant x

instance Traversable (Constant a) where
	Constant x ->> _ = point (Constant x)

instance Setoid a => Setoid (Constant a b) where
	Constant x == Constant y = x == y

instance Chain a => Chain (Constant a b) where
	Constant x <=> Constant y = x <=> y

instance Semigroup a => Semigroup (Constant a b) where
	Constant x + Constant y = Constant $ x + y

instance Monoid a => Monoid (Constant a b) where
	 unit = Constant unit

instance Ringoid a => Ringoid (Constant a b) where
	Constant x * Constant y = Constant $ x * y

instance Infimum a => Infimum (Constant a b) where
	Constant x /\ Constant y = Constant $ x /\ y

instance Supremum a => Supremum (Constant a b) where
	Constant x \/ Constant y = Constant $ x \/ y

instance Lattice a => Lattice (Constant a b) where

instance Group a => Group (Constant a b) where
	inverse (Constant x) = Constant $ inverse x
