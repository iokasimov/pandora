module Pandora.Paradigm.Basis.Product (Product (..), type (:*:), Has, Injective
	, delta, swap, attached, curry, uncurry) where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)), (&&))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (inverse))
import Pandora.Pattern.Functor.Divariant (($))

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

instance (Setoid a, Setoid b) => Setoid (Product a b) where
	(x :*: y) == (x' :*: y') = x == x' && y == y'

instance (Semigroup a, Semigroup b) => Semigroup (Product a b) where
	(x :*: y) + (x' :*: y') = x + x' :*: y + y'

instance (Monoid a, Monoid b) => Monoid (Product a b) where
	zero = zero :*: zero

instance (Ringoid a, Ringoid b) => Ringoid (Product a b) where
	(x :*: y) * (x' :*: y') = x * x' :*: y * y'

instance (Infimum a, Infimum b) => Infimum (Product a b) where
	(x :*: y) /\ (x' :*: y') = x /\ x' :*: y /\ y'

instance (Supremum a, Supremum b) => Supremum (Product a b) where
	(x :*: y) \/ (x' :*: y') = x \/ x' :*: y \/ y'

instance (Lattice a, Lattice b) => Lattice (Product a b) where

instance (Group a, Group b) => Group (Product a b) where
	inverse (x :*: y) = inverse x :*: inverse y

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

-- Constraint on the content of some type
type family Has x xs where
	Has x (x :*: xs) = ()
	Has x (y :*: xs) = Has x xs
	Has x x = ()

-- All elements of the left product are in the right product
type family Injective xs ys where
	Injective (x :*: xs) ys = (Has x ys, Injective xs ys)
	Injective x (x :*: ys) = ()
	Injective x (y :*: ys) = Has x ys
	Injective x x = ()
