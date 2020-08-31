module Pandora.Paradigm.Primary.Functor.Validation where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Greater))

data Validation e a = Flaws e | Validated a

instance Covariant (Validation e) where
	_ <$> Flaws e = Flaws e
	f <$> Validated x = Validated $ f x

instance Pointable (Validation e) where
	point = Validated

instance Semigroup e => Applicative (Validation e) where
	Flaws e <*> Flaws e' = Flaws $ e + e'
	Flaws e <*> Validated _ = Flaws e
	Validated _ <*> Flaws e' = Flaws e'
	Validated f <*> Validated x = Validated $ f x

instance Alternative (Validation e) where
	Flaws _ <+> x = x
	Validated x <+> _ = Validated x

instance Traversable (Validation e) where
	Validated x ->> f = Validated <$> f x
	Flaws e ->> _ = point $ Flaws e

instance Bivariant Validation where
	f <-> g = validation (Flaws . f) (Validated . g)

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
