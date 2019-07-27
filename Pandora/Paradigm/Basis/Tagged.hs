module Pandora.Paradigm.Basis.Tagged (Tagged (..), untag, retag, tagself) where

import Pandora.Core.Morphism ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (inverse))

newtype Tagged tag a = Tagged a

instance Covariant (Tagged tag) where
	f <$> Tagged x = Tagged $ f x

instance Pointable (Tagged tag) where
	point = Tagged

instance Extractable (Tagged tag) where
	extract (Tagged x) = x

instance Applicative (Tagged tag) where
	Tagged f <*> Tagged x = Tagged $ f x

instance Traversable (Tagged tag) where
	Tagged x ->> f = Tagged <$> f x

instance Distributive (Tagged tag) where
	x >>- f = Tagged $ extract . f <$> x

instance Bindable (Tagged tag) where
	Tagged x >>= f = f x

instance Monad (Tagged tag)

instance Extendable (Tagged tag) where
	x =>> f = Tagged . f $ x

instance Comonad (Tagged tag)

instance Setoid a => Setoid (Tagged tag a) where
	Tagged x == Tagged y = x == y

instance Chain a => Chain (Tagged tag a) where
	Tagged x <=> Tagged y = x <=> y

instance Semigroup a => Semigroup (Tagged tag a) where
	Tagged x + Tagged y = Tagged $ x + y

instance Monoid a => Monoid (Tagged tag a) where
	 zero = Tagged zero

instance Ringoid a => Ringoid (Tagged tag a) where
	Tagged x * Tagged y = Tagged $ x * y

instance Infimum a => Infimum (Tagged tag a) where
	Tagged x /\ Tagged y = Tagged $ x /\ y

instance Supremum a => Supremum (Tagged tag a) where
	Tagged x \/ Tagged y = Tagged $ x \/ y

instance Lattice a => Lattice (Tagged tag a) where

instance Group a => Group (Tagged tag a) where
	inverse (Tagged x) = Tagged $ inverse x

untag :: Tagged tag a -> a
untag (Tagged x) = x

retag :: Tagged old a -> Tagged new a
retag (Tagged x) = Tagged x

tagself :: a -> Tagged a a
tagself = Tagged
