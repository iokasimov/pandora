module Pandora.Paradigm.Primary.Functor.Validation where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply_))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Sum ((:+:) (Option, Adoption))
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Greater))

data Validation e a = Flaws e | Validated a

instance Covariant (Validation e) (->) (->) where
	_ -<$>- Flaws e = Flaws e
	f -<$>- Validated x = Validated $ f x
	_ -<$>- Flaws e = Flaws e
	f -<$>- Validated x = Validated $ f x
	_ -<$>- Flaws e = Flaws e
	f -<$>- Validated x = Validated $ f x

instance Covariant (Flip Validation a) (->) (->) where
	f -<$>- Flip (Flaws e) = Flip . Flaws $ f e
	_ -<$>- Flip (Validated x) = Flip $ Validated x
	f -<$>- Flip (Flaws e) = Flip . Flaws $ f e
	_ -<$>- Flip (Validated x) = Flip $ Validated x
	f -<$>- Flip (Flaws e) = Flip . Flaws $ f e
	_ -<$>- Flip (Validated x) = Flip $ Validated x

instance Pointable (Validation e) (->) where
	point = Validated

instance Semigroup e => Semimonoidal (Validation e) (->) (:*:) (:*:) where
	multiply_ (Validated x :*: Validated y) = Validated $ x :*: y
	multiply_ (Flaws x :*: Flaws y) = Flaws $ x + y
	multiply_ (Validated _ :*: Flaws y) = Flaws y
	multiply_ (Flaws x :*: Validated _) = Flaws x

instance Semigroup e => Semimonoidal (Validation e) (->) (:*:) (:+:) where
	multiply_ (Flaws _ :*: y) = Adoption -<$>- y
	multiply_ (Validated x :*: _) = Option -<$>- Validated x

instance Traversable (Validation e) (->) (->) where
	f <<- Validated x = Validated -<$>- f x
	_ <<- Flaws e = point $ Flaws e

instance Bivariant Validation (->) (->) (->) where
	f <-> g = validation # Flaws . f # Validated . g

instance (Setoid e, Setoid a) => Setoid (Validation e a) where
	Validated x == Validated y = x == y
	Flaws x == Flaws y = x == y
	_ == _ = False

instance (Chain e, Chain a) => Chain (Validation e a) where
	Validated x <=> Validated y = x <=> y
	Flaws x <=> Flaws y = x <=> y
	Flaws _ <=> Validated _ = Less
	Validated _ <=> Flaws _ = Greater

instance (Semigroup e, Semigroup a) => Semigroup (Validation e a) where
	Validated x + Validated y = Validated $ x + y
	Flaws x + Flaws y = Flaws $ x + y
	Flaws _ + Validated y = Validated y
	Validated x + Flaws _ = Validated x

validation :: (e -> r) -> (a -> r) -> Validation e a -> r
validation f _ (Flaws x) = f x
validation _ s (Validated x) = s x
