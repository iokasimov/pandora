module Pandora.Paradigm.Primary.Functor.Tagged where

import Pandora.Core.Functor (type (:=>), type (~>))
import Pandora.Core.Interpreted ((<~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Morphism.Kleisli (Kleisli (Kleisli))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor (Functor ((-|-)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Pattern.Object.Semilattice (Infimum ((/\)), Supremum ((\/)))
import Pandora.Pattern.Object.Lattice (Lattice)
import Pandora.Pattern.Object.Group (Group (invert))
import Pandora.Pattern.Operation.Exponential (type (--<), type (-->))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Pattern.Operation.One (One (One))
import Pandora.Paradigm.Algebraic (extract, (<<-|-))

newtype Tagged tag a = Tag a

instance Functor (-->) (-->) (Tagged tag) where
	(-|-) (Straight f) = Straight <-- \case
		Tag x -> Tag <-- f x

instance Functor (Kleisli (Tagged tag) (->)) (-->) (Tagged tag) where
	(-|-) (Kleisli f) = Straight <-- \case
		Tag x -> f x

infixr 0 :#
type (:#) tag = Tagged tag

instance Covariant (->) (->) (Tagged tag) where
	f <-|- Tag x = Tag <-- f x

instance Covariant (->) (->) (Flip Tagged a) where
	_ <-|- Flip (Tag x) = Flip <-- Tag x

instance Semimonoidal (-->) (:*:) (:*:) (Tagged tag) where
	mult = Straight <-- Tag . (extract <<-|-) . (extract <-|-)

instance Monoidal (-->) (-->) (:*:) (:*:) (Tagged tag) where
	unit _ = Straight <-- Tag . (<~ One)

instance Semimonoidal (--<) (:*:) (:*:) (Tagged tag) where
	mult = Flip <-- \(Tag (x :*: y)) -> Tag x :*: Tag y

instance Monoidal (--<) (-->) (:*:) (:*:) (Tagged tag) where
	unit _ = Flip <-- \(Tag x) -> Straight (\_ -> x)

instance Traversable (->) (->) (Tagged tag) where
	f <-/- Tag x = Tag <-|- f x

instance Distributive (->) (->) (Tagged tag) where
	f -<< x = Tag <---- extract . f <-|- x

instance Bindable (->) (Tagged tag) where
	f =<< Tag x = f x

instance Monad (->) (Tagged tag)

instance Extendable (->) (Tagged tag) where
	f <<= x = Tag <-- f x

instance Comonad (->) (Tagged tag)

instance Setoid a => Setoid (Tagged tag a) where
	Tag x == Tag y = x == y

instance Chain a => Chain (Tagged tag a) where
	Tag x <=> Tag y = x <=> y

instance Semigroup a => Semigroup (Tagged tag a) where
	Tag x + Tag y = Tag <-- x + y

instance Monoid a => Monoid (Tagged tag a) where
	 zero = Tag zero

instance Ringoid a => Ringoid (Tagged tag a) where
	Tag x * Tag y = Tag <--- x * y

instance Quasiring a => Quasiring (Tagged tag a) where
	one = Tag one

instance Infimum a => Infimum (Tagged tag a) where
	Tag x /\ Tag y = Tag <-- x /\ y

instance Supremum a => Supremum (Tagged tag a) where
	Tag x \/ Tag y = Tag <-- x \/ y

instance Lattice a => Lattice (Tagged tag a) where

instance Group a => Group (Tagged tag a) where
	invert (Tag x) = Tag <-- invert x

retag :: forall new old . Tagged old ~> Tagged new
retag (Tag x) = Tag x

tagself :: a :=> Tagged a
tagself = Tag
