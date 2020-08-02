module Pandora.Paradigm.Primary.Functor.Product (Product (..), type (:*:)
	, delta, swap, attached, curry, uncurry) where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
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

data Product a b = a :*: b

type (:*:) = Product

instance Covariant (Product a) where
	f <$> (x :*: y) = x :*: f y

instance Extractable (Product a) where
	extract (_ :*: y) = y

instance Traversable (Product a) where
	(x :*: y) ->> f = (:*:) x <$> f y

instance Extendable (Product a) where
	(x :*: y) =>> f = (:*:) x $ f (x :*: y)

instance Comonad (Product a) where

instance Adjoint (Product a) ((->) a) where
	x -| f = \y -> f $ y :*: x
	(y :*: x) |- f = f x y

instance Bivariant Product where
	f <-> g = \(x :*: y) -> f x :*: g y

instance (Setoid a, Setoid b) => Setoid (Product a b) where
	(x :*: y) == (x' :*: y') = (x == x') * (y == y')

instance (Semigroup a, Semigroup b) => Semigroup (Product a b) where
	(x :*: y) + (x' :*: y') = x + x' :*: y + y'

instance (Monoid a, Monoid b) => Monoid (Product a b) where
	zero = zero :*: zero

instance (Ringoid a, Ringoid b) => Ringoid (Product a b) where
	(x :*: y) * (x' :*: y') = x * x' :*: y * y'

instance (Quasiring a, Quasiring b) => Quasiring (Product a b) where
	one = one :*: one

instance (Infimum a, Infimum b) => Infimum (Product a b) where
	(x :*: y) /\ (x' :*: y') = x /\ x' :*: y /\ y'

instance (Supremum a, Supremum b) => Supremum (Product a b) where
	(x :*: y) \/ (x' :*: y') = x \/ x' :*: y \/ y'

instance (Lattice a, Lattice b) => Lattice (Product a b) where

instance (Group a, Group b) => Group (Product a b) where
	invert (x :*: y) = invert x :*: invert y

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
