module Pandora.Paradigm.Primary.Functor.Identity where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply_))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
--import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
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

newtype Identity a = Identity a

instance Covariant Identity (->) (->) where
	f -<$>- Identity x = Identity $ f x

instance Semimonoidal Identity (->) (:*:) (:*:) where
	multiply_ (Identity x :*: Identity y) = Identity $ x :*: y

instance Monoidal Identity (->) (->) (:*:) (:*:) where
	unit _ f = Identity $ f One

instance Semimonoidal Identity (<--) (:*:) (:*:) where
	multiply_ = Flip $ \(Identity (x :*: y)) -> Identity x :*: Identity y

instance Monoidal Identity (<--) (->) (:*:) (:*:) where
	unit _ = Flip $ \(Identity x) -> (\_ -> x)

instance Traversable Identity (->) (->) where
	f <<- Identity x = Identity -<$>- f x

instance Bindable Identity (->) where
	f =<< Identity x = f x	

instance Monad Identity

instance Extendable Identity (->) where
	f <<= x = Identity . f $ x

instance Comonad Identity (->)

--instance Representable Identity where
	--type Representation Identity = ()
	--() <#> Identity x = x
	--tabulate f = Identity $ f ()

instance Adjoint Identity Identity (->) (->) where
	f -| x = Identity . f . Identity $ x
	g |- x = extract . extract . (g -<$>-) $ x

instance Setoid a => Setoid (Identity a) where
	Identity x == Identity y = x == y

instance Chain a => Chain (Identity a) where
	Identity x <=> Identity y = x <=> y

instance Semigroup a => Semigroup (Identity a) where
	Identity x + Identity y = Identity $ x + y

instance Monoid a => Monoid (Identity a) where
	 zero = Identity zero

instance Ringoid a => Ringoid (Identity a) where
	Identity x * Identity y = Identity $ x * y

instance Quasiring a => Quasiring (Identity a) where
	 one = Identity one

instance Infimum a => Infimum (Identity a) where
	Identity x /\ Identity y = Identity $ x /\ y

instance Supremum a => Supremum (Identity a) where
	Identity x \/ Identity y = Identity $ x \/ y

instance Lattice a => Lattice (Identity a) where

instance Group a => Group (Identity a) where
	invert (Identity x) = Identity $ invert x
