module Paradigm.Basis.Identity (Identity (..)) where

import Core.Morphism ((.), ($))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Applicative (Applicative ((<*>)))
import Pattern.Functor.Traversable (Traversable ((->>)))
import Pattern.Functor.Distributive (Distributive ((>>-)))
import Pattern.Functor.Bindable (Bindable ((>>=)))
import Pattern.Functor.Extendable (Extendable ((=>>)))
import Pattern.Functor.Adjoint (Adjoint (phi, psi))
import Pattern.Object.Setoid (Setoid ((==)))
import Pattern.Object.Chain (Chain ((<=)))
import Pattern.Object.Semigroup (Semigroup ((<>)))
import Pattern.Object.Monoid (Monoid (unit))
import Pattern.Object.Ringoid (Ringoid ((><)))
import Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pattern.Object.Lattice (Lattice)
import Pattern.Object.Group (Group (inverse))

newtype Identity a = Identity a

instance Covariant Identity where
	f <$> Identity x = Identity $ f x

instance Applicative Identity where
	Identity f <*> Identity x = Identity $ f x

instance Pointable Identity where
	point = Identity

instance Extractable Identity where
	extract (Identity x) = x

instance Traversable Identity where
	Identity x ->> f = Identity <$> f x

instance Distributive Identity where
	x >>- f = Identity $ extract . f <$> x

instance Bindable Identity where
	Identity x >>= f = f x

instance Extendable Identity where
	x =>> f = Identity . f $ x

instance Adjoint Identity Identity where
	phi f = Identity . f . Identity
	psi f = extract . extract . comap f

instance Setoid a => Setoid (Identity a) where
	Identity x == Identity y = x == y

instance Chain a => Chain (Identity a) where
	Identity x <= Identity y = x <= y

instance Semigroup a => Semigroup (Identity a) where
	Identity x <> Identity y = Identity $ x <> y

instance Monoid a => Monoid (Identity a) where
	 unit = Identity unit

instance Ringoid a => Ringoid (Identity a) where
	Identity x >< Identity y = Identity $ x >< y

instance Infimum a => Infimum (Identity a) where
	Identity x /\ Identity y = Identity $ x /\ y

instance Supremum a => Supremum (Identity a) where
	Identity x \/ Identity y = Identity $ x \/ y

instance Lattice a => Lattice (Identity a) where

instance Group a => Group (Identity a) where
	inverse (Identity x) = Identity $ inverse x
