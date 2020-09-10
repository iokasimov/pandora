module Pandora.Paradigm.Primary.Functor.Product where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (invert))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (iterate))

infixr 1 :*:

data Product s a = s :*: a

type (:*:) = Product

instance Covariant (Product s) where
	f <$> (s :*: x) = s :*: f x

instance Extractable (Product a) where
	extract (_ :*: y) = y

instance Traversable (Product s) where
	(s :*: x) ->> f = (s :*:) <$> f x

instance Extendable (Product s) where
	(s :*: x) =>> f = s :*: f (s :*: x)

instance Comonad (Product s) where

instance Bivariant Product where
	f <-> g = \(s :*: x) -> f s :*: g x

instance (Setoid s, Setoid a) => Setoid (Product s a) where
	(s :*: x) == (s' :*: x') = (s == s') * (x == x')

instance (Semigroup s, Semigroup a) => Semigroup (Product s a) where
	(s :*: x) + (s' :*: x') = s + s' :*: x + x'

instance (Monoid s, Monoid a) => Monoid (Product s a) where
	zero = zero :*: zero

instance (Ringoid s, Ringoid a) => Ringoid (Product s a) where
	(s :*: x) * (s' :*: x') = s * s' :*: x * x'

instance (Quasiring s, Quasiring a) => Quasiring (Product s a) where
	one = one :*: one

instance (Infimum s, Infimum a) => Infimum (Product s a) where
	(s :*: x) /\ (s' :*: x') = s /\ s' :*: x /\ x'

instance (Supremum s, Supremum a) => Supremum (Product s a) where
	(s :*: x) \/ (s' :*: x') = s \/ s' :*: x \/ x'

instance (Lattice s, Lattice a) => Lattice (Product s a) where

instance (Group s, Group a) => Group (Product s a) where
	invert (s :*: x) = invert s :*: invert x

instance Monotonic e a => Monotonic (Product a e) a where
	iterate f r (x :*: e) = iterate f (f x r) e

delta :: a -> a :*: a
delta x = x :*: x

swap :: a :*: b -> b :*: a
swap (x :*: y) = y :*: x

attached :: a :*: b -> a
attached (x :*: _) = x

curry :: (a :*: b -> c) -> a -> b -> c
curry f x y = f $ x :*: y

uncurry :: (a -> b -> c) -> (a :*: b -> c)
uncurry f (x :*: y) = f x y
