module Pandora.Paradigm.Basis.Jet (Jet (..)) where

import Pandora.Core.Morphism ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), comap)
import Pandora.Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), traverse))

infixr 5 :-

data Jet t a = a :- Jet t (t a)

instance Covariant t => Covariant (Jet t) where
	f <$> a :- as = f a :- (comap . comap) f as

instance Traversable t => Traversable (Jet t) where
	(a :- as) ->> f = (:-) <$> f a <*> (traverse . traverse) f as

instance (forall t' . Exclusive t') => Pointable (Jet t) where
	point x = x :- exclusive

instance Covariant t => Extractable (Jet t) where
	extract (x :- _) = x
