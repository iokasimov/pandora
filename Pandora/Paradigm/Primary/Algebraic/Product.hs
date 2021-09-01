module Pandora.Paradigm.Primary.Algebraic.Product where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (invert))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))

infixr 0 :*:

data (:*:) s a = s :*: a

instance Covariant (->) (->) ((:*:) s) where
	f <$> ~(s :*: x) = s :*: f x

instance Covariant (->) (->) (Flip (:*:) a) where
	f <$> (Flip (x :*: y)) = Flip $ f x :*: y

instance Extendable (->) ((:*:) s) where
	f <<= ~(s :*: x) = s :*: f (s :*: x)

instance Bivariant (->) (->) (->) (:*:) where
	f <-> g = \ ~(s :*: x) -> f s :*: g x

instance (Setoid s, Setoid a) => Setoid (s :*: a) where
	~(sx :*: x) == ~(sy :*: y) = (sx == sy) * (x == y)

instance (Semigroup s, Semigroup a) => Semigroup (s :*: a) where
	~(sx :*: x) + ~(sy :*: y) = sx + sy :*: x + y

instance (Monoid s, Monoid a) => Monoid (s :*: a) where
	zero = zero :*: zero

instance (Ringoid s, Ringoid a) => Ringoid (s :*: a) where
	~(sx :*: x) * ~(sy :*: y) = sx * sy :*: x * y

instance (Quasiring s, Quasiring a) => Quasiring (s :*: a) where
	one = one :*: one

instance (Infimum s, Infimum a) => Infimum (s :*: a) where
	~(sx :*: x) /\ ~(sy :*: y) = sx /\ sy :*: x /\ y

instance (Supremum s, Supremum a) => Supremum (s :*: a) where
	~(sx :*: x) \/ ~(sy :*: y) = sx \/ sy :*: x \/ y

instance (Lattice s, Lattice a) => Lattice (s :*: a) where

instance (Group s, Group a) => Group (s :*: a) where
	invert ~(s :*: x) = invert # s :*: invert # x

instance {-# OVERLAPS #-} Semimonoidal (->) (:*:) (:*:) t => Semimonoidal (->) (:*:) (:*:) (t <:.:> t := (:*:)) where
	multiply (T_U (xls :*: xrs) :*: T_U (yls :*: yrs)) = T_U $ multiply (xls :*: yls) :*: multiply (xrs :*: yrs)

-- TODO: Generalize (:*:) as Bivariant p
instance (Semimonoidal (<--) (:*:) (:*:) t, Semimonoidal (<--) (:*:) (:*:) u) => Semimonoidal (<--) (:*:) (:*:) (t <:.:> u := (:*:)) where
	multiply = Flip $ \(T_U (lxys :*: rxys)) ->
		let Flip f = multiply @(<--) @(:*:) @(:*:) in
		let Flip g = multiply @(<--) @(:*:) @(:*:) in
		let (lxs :*: lys) = f lxys in
		let (rxs :*: rys) = g rxys in
		T_U (lxs :*: rxs) :*: T_U (lys :*: rys)

delta :: a -> a :*: a
delta x = x :*: x

swap :: a :*: b -> b :*: a
swap ~(x :*: y) = y :*: x

attached :: a :*: b -> a
attached ~(x :*: _) = x

twosome :: t a -> u a -> (<:.:>) t u (:*:) a
twosome x y = T_U $ x :*: y
