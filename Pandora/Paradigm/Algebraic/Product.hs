module Pandora.Paradigm.Algebraic.Product where

import Pandora.Core.Functor (type (>>>>>>))
import Pandora.Pattern.Category ((<---))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (invert))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>), type (>:.:>), type (<:.:<), type (>:.:<))

-- TODO: Change precedence to 7
infixr 8 :*:
infixr 5 <:*:>

data (:*:) s a = s :*: a

instance Covariant (->) (->) ((:*:) s) where
	f <-|- ~(s :*: x) = s :*: f x

instance Covariant (->) (->) (Flip (:*:) a) where
	f <-|- Flip (x :*: y) = Flip (f x :*: y)

instance Extendable (->) ((:*:) s) where
	f <<= ~(s :*: x) = s :*: f (s :*: x)

instance (Setoid s, Setoid a) => Setoid (s :*: a) where
	~(sx :*: x) == ~(sy :*: y) = (sx == sy) * (x == y)

instance (Semigroup s, Semigroup a) => Semigroup (s :*: a) where
	~(sx :*: x) + ~(sy :*: y) = (sx + sy) :*: (x + y)

instance (Monoid s, Monoid a) => Monoid (s :*: a) where
	zero = zero :*: zero

instance (Ringoid s, Ringoid a) => Ringoid (s :*: a) where
	~(sx :*: x) * ~(sy :*: y) = (sx * sy) :*: (x * y)

instance (Quasiring s, Quasiring a) => Quasiring (s :*: a) where
	one = one :*: one

instance (Infimum s, Infimum a) => Infimum (s :*: a) where
	~(sx :*: x) /\ ~(sy :*: y) = sx /\ sy :*: x /\ y

instance (Supremum s, Supremum a) => Supremum (s :*: a) where
	~(sx :*: x) \/ ~(sy :*: y) = sx \/ sy :*: x \/ y

instance (Lattice s, Lattice a) => Lattice (s :*: a) where

instance (Group s, Group a) => Group (s :*: a) where
	invert ~(s :*: x) = invert s :*: invert x

delta :: a -> a :*: a
delta x = x :*: x

swap :: a :*: b -> b :*: a
swap ~(x :*: y) = y :*: x

attached :: a :*: b -> a
attached ~(x :*: _) = x

type (<:*:>) t u = t <:.:> u >>>>>> (:*:)
type (>:*:>) t u = t >:.:> u >>>>>> (:*:)
type (<:*:<) t u = t <:.:< u >>>>>> (:*:)
type (>:*:<) t u = t >:.:< u >>>>>> (:*:)

(<:*:>) :: t a -> u a -> t <:*:> u >>>>>> a
(<:*:>) xs ys = T_U <--- xs :*: ys
