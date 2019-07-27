module Pandora.Paradigm.Basis.Validation (Validation (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))

data Validation e a = Flaws e | Validated a

instance Covariant (Validation e) where
	_ <$> Flaws e = Flaws e
	f <$> Validated x = Validated $ f x

instance Pointable (Validation e) where
	point = Validated

instance Semigroup e => Applicative (Validation e) where
	Flaws e <*> Flaws e' = Flaws $ e + e'
	Flaws e <*> Validated _ = Flaws e
	Validated _ <*> Flaws e2 = Flaws e2
	Validated f <*> Validated x = Validated $ f x

instance Alternative (Validation e) where
	Flaws _ <+> x = x
	Validated x <+> _ = Validated x

instance Traversable (Validation e) where
	Validated x ->> f = Validated <$> f x
	Flaws e ->> _ = point $ Flaws e
