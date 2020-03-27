module Pandora.Paradigm.Basis.Jet (Jet (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), (<$$>))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))

data Jet t a = Jet a (Jet t (t a))

instance Covariant t => Covariant (Jet t) where
	f <$> Jet a as = Jet (f a) (f <$$> as)

instance Traversable t => Traversable (Jet t) where
	Jet a as ->> f = Jet <$> f a <*> as ->>> f

instance (forall u . Avoidable u) => Pointable (Jet t) where
	point x = Jet x empty

instance Covariant t => Extractable (Jet t) where
	extract (Jet x _) = x
