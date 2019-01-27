module Pandora.Paradigm.Basis.Identity (Identity (..)) where

import Pandora.Core.Morphism ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Adjoint (Adjoint (phi, psi))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((<>)))
import Pandora.Pattern.Object.Monoid (Monoid (unit))
import Pandora.Pattern.Object.Ringoid (Ringoid ((><)))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (inverse))

newtype Identity a = Identity a

instance Covariant Identity where
	f <$> Identity x = Identity $ f x

instance Pointable Identity where
	point = Identity

instance Extractable Identity where
	extract (Identity x) = x

instance Applicative Identity where
	Identity f <*> Identity x = Identity $ f x

instance Traversable Identity where
	Identity x ->> f = Identity <$> f x

instance Distributive Identity where
	x >>- f = Identity $ extract . f <$> x

instance Bindable Identity where
	Identity x >>= f = f x

instance Monad Identity

instance Extendable Identity where
	x =>> f = Identity . f $ x

instance Comonad Identity

instance Adjoint Identity Identity where
	phi f = Identity . f . Identity
	psi f = extract . extract . comap f

instance Setoid a => Setoid (Identity a) where
	Identity x == Identity y = x == y

instance Chain a => Chain (Identity a) where
	Identity x <=> Identity y = x <=> y

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
