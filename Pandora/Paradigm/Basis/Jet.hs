module Pandora.Paradigm.Basis.Jet (Jet (..)) where

import Pandora.Core.Morphism ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), comap)
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), traverse))

data Jet t a = a :- Jet t (t a)

instance Covariant t => Covariant (Jet t) where
	f <$> a :- as = f a :- (comap . comap) f as

instance Traversable t => Traversable (Jet t) where
	a :- as ->> f = (:-) <$> f a <*> (traverse . traverse) f as
