module Pandora.Paradigm.Primary.Functor.Tagged where

import Pandora.Core.Functor (type (:=>), type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (invert))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (extract)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))

newtype Tagged tag a = Tag a

infixr 0 :#
type (:#) tag = Tagged tag

instance Covariant (Tagged tag) (->) (->) where
	f -<$>- Tag x = Tag $ f x

instance Covariant (Flip Tagged a) (->) (->) where
	_ -<$>- Flip (Tag x) = Flip $ Tag x

instance Semimonoidal (Tagged tag) (->) (:*:) (:*:) where
	multiply (x :*: y) = Tag $ extract x :*: extract y

instance Monoidal (Tagged tag) (->) (->) (:*:) (:*:) where
	unit _ f = Tag $ f One

instance Semimonoidal (Tagged tag) (<--) (:*:) (:*:) where
	multiply = Flip $ \(Tag (x :*: y)) -> Tag x :*: Tag y

instance Monoidal (Tagged tag) (<--) (->) (:*:) (:*:) where
	unit _ = Flip $ \(Tag x) -> (\_ -> x)

instance Traversable (Tagged tag) (->) (->) where
	f <<- Tag x = Tag -<$>- f x

instance Distributive (Tagged tag) (->) (->) where
	f -<< x = Tag $ extract . f -<$>- x

instance Bindable (Tagged tag) (->) where
	f =<< Tag x = f x

instance Monad (Tagged tag)

instance Extendable (Tagged tag) (->) where
	f <<= x = Tag . f $ x

instance Comonad (Tagged tag) (->)

instance Bivariant Tagged (->) (->) (->) where
	_ <-> g = \(Tag x) -> Tag $ g x

instance Setoid a => Setoid (Tagged tag a) where
	Tag x == Tag y = x == y

instance Chain a => Chain (Tagged tag a) where
	Tag x <=> Tag y = x <=> y

instance Semigroup a => Semigroup (Tagged tag a) where
	Tag x + Tag y = Tag $ x + y

instance Monoid a => Monoid (Tagged tag a) where
	 zero = Tag zero

instance Ringoid a => Ringoid (Tagged tag a) where
	Tag x * Tag y = Tag $ x * y

instance Quasiring a => Quasiring (Tagged tag a) where
	one = Tag one

instance Infimum a => Infimum (Tagged tag a) where
	Tag x /\ Tag y = Tag $ x /\ y

instance Supremum a => Supremum (Tagged tag a) where
	Tag x \/ Tag y = Tag $ x \/ y

instance Lattice a => Lattice (Tagged tag a) where

instance Group a => Group (Tagged tag a) where
	invert (Tag x) = Tag $ invert x

retag :: forall new old . Tagged old ~> Tagged new
retag (Tag x) = Tag x

tagself :: a :=> Tagged a
tagself = Tag
