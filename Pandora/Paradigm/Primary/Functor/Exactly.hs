module Pandora.Paradigm.Primary.Functor.Exactly where

import Pandora.Core.Interpreted ((<~))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Morphism.Kleisli (Kleisli (Kleisli))
import Pandora.Pattern.Morphism.Tensor (Tensor (Tensor))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Functor (Functor ((-|-)))
import Pandora.Pattern.Transformation (Component (component))
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

newtype Exactly a = Exactly a

instance Functor (-->) (-->) Exactly where
	(-|-) (Straight f) = Straight <-- \case
		Exactly x -> Exactly <-- f x

instance Functor (Kleisli Exactly (->)) (-->) Exactly where
	(-|-) (Kleisli f) = Straight <-- \case
		Exactly x -> f x

instance Component (Tensor (:*:) (-->) (:*:)) Exactly Exactly where
	component = Tensor . Straight <-- \(Exactly l :*: Exactly r) -> Exactly (l :*: r)

instance Component (Tensor (:*:) (--<) (:*:)) Exactly Exactly where
	component = Tensor . Flip <-- \(Exactly (l :*: r)) -> Exactly l :*: Exactly r

instance Covariant (->) (->) Exactly where
	f <-|- Exactly x = Exactly <-- f x

instance Semimonoidal (-->) (:*:) (:*:) Exactly where
	mult = Straight <-- Exactly . (extract <<-|-) .  (extract <-|-)

instance Monoidal (-->) (-->) (:*:) (:*:) Exactly where
	unit _ = Straight <-- Exactly . (<~ One)

instance Semimonoidal (--<) (:*:) (:*:) Exactly where
	mult = Flip <-- \(Exactly (x :*: y)) -> Exactly x :*: Exactly y

instance Monoidal (--<) (-->) (:*:) (:*:) Exactly where
	unit _ = Flip <-- \(Exactly x) -> Straight (\_ -> x)

instance Traversable (->) (->) Exactly where
	f <-/- Exactly x = Exactly <-|- f x

instance Bindable (->) Exactly where
	f =<< Exactly x = f x

instance Monad (->) Exactly

instance Extendable (->) Exactly where
	f <<= x = Exactly . f <-- x

instance Comonad (->) Exactly

instance Representable Exactly where
	type Representation Exactly = ()
	() <#> Exactly x = x
	tabulate f = Exactly <-- f ()

instance Adjoint (->) (->) Exactly Exactly where
	f -| x = Exactly . f . Exactly <-- x
	g |- x = extract . extract <---- g <-|- x

instance Setoid a => Setoid (Exactly a) where
	Exactly x == Exactly y = x == y

instance Chain a => Chain (Exactly a) where
	Exactly x <=> Exactly y = x <=> y

instance Semigroup a => Semigroup (Exactly a) where
	Exactly x + Exactly y = Exactly <-- x + y

instance Monoid a => Monoid (Exactly a) where
	 zero = Exactly zero

instance Ringoid a => Ringoid (Exactly a) where
	Exactly x * Exactly y = Exactly <--- x * y

instance Quasiring a => Quasiring (Exactly a) where
	 one = Exactly one

instance Infimum a => Infimum (Exactly a) where
	Exactly x /\ Exactly y = Exactly <-- x /\ y

instance Supremum a => Supremum (Exactly a) where
	Exactly x \/ Exactly y = Exactly <-- x \/ y

instance Lattice a => Lattice (Exactly a) where

instance Group a => Group (Exactly a) where
	invert (Exactly x) = Exactly <-- invert x
