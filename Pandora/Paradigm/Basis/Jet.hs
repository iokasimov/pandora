module Pandora.Paradigm.Basis.Jet (Jet (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), (<$$>))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))

infixr 6 :-

data Jet t a = a :- Jet t (t a)

instance Covariant t => Covariant (Jet t) where
	f <$> a :- as = f a :- f <$$> as

instance Traversable t => Traversable (Jet t) where
	a :- as ->> f = (:-) <$> f a <*> as ->>> f

instance (forall t' . Avoidable t') => Pointable (Jet t) where
	point x = x :- empty

instance Covariant t => Extractable (Jet t) where
	extract (x :- _) = x
