module Pandora.Paradigm.Primary.Functor.Product where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)), Extendable_ (duplicate_))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)), Bivariant_ ((-<->-)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (invert))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))

infixr 0 :*:

data Product s a = s :*: a

type (:*:) = Product

instance Covariant (Product s) where
	f <$> x = attached x :*: f # extract x

instance Covariant_ (Product s) (->) (->) where
	f -<$>- x = attached x :*: f # extract x

instance Extractable (Product a) where
	extract ~(_ :*: y) = y

instance Traversable (Product s) where
	x ->> f = (attached x :*:) <$> f (extract x)

instance Extendable (Product s) where
	x =>> f = attached x :*: f (attached x :*: extract x)

instance Extendable_ (Product s) (->) where
	duplicate_ (s :*: x) = s :*: (s :*: x) 

instance Comonad (Product s) where

instance Bivariant Product where
	f <-> g = \ ~(s :*: x) -> f s :*: g x

instance Bivariant_ Product (->) (->) (->) where
	f -<->- g = \ ~(s :*: x) -> f s :*: g x

instance (Setoid s, Setoid a) => Setoid (s :*: a) where
	x == y = (attached x == attached y) * (extract x == extract y)

instance (Semigroup s, Semigroup a) => Semigroup (s :*: a) where
	x + y = attached x + attached y :*: extract x + extract y

instance (Monoid s, Monoid a) => Monoid (s :*: a) where
	zero = zero :*: zero

instance (Ringoid s, Ringoid a) => Ringoid (s :*: a) where
	x * y = attached x * attached y :*: extract x * extract y

instance (Quasiring s, Quasiring a) => Quasiring (s :*: a) where
	one = one :*: one

instance (Infimum s, Infimum a) => Infimum (s :*: a) where
	x /\ y = attached x /\ attached y :*: extract x /\ extract y

instance (Supremum s, Supremum a) => Supremum (s :*: a) where
	x \/ y = attached x \/ attached y :*: extract x \/ extract y

instance (Lattice s, Lattice a) => Lattice (s :*: a) where

instance (Group s, Group a) => Group (s :*: a) where
	invert x = invert # attached x :*: invert # extract x

instance {-# OVERLAPS #-} Applicative t => Applicative (t <:.:> t := (:*:)) where
	T_U (lfs :*: rfs) <*> T_U (ls :*: rs) = T_U $ lfs <*> ls :*: rfs <*> rs

delta :: a -> a :*: a
delta x = x :*: x

swap :: a :*: b -> b :*: a
swap ~(x :*: y) = y :*: x

attached :: a :*: b -> a
attached ~(x :*: _) = x

twosome :: t a -> u a -> (<:.:>) t u (:*:) a
twosome x y = T_U $ x :*: y
