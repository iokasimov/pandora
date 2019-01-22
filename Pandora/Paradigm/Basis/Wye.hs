module Pandora.Paradigm.Basis.Wye (Wye (..)) where

import Pandora.Core.Morphism (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))

data Wye a = End | Left a | Right a | Both a a

instance Covariant Wye where
	f <$> End = End
	f <$> Left x = Left $ f x
	f <$> Right y = Right $ f y
	f <$> Both x y = Both (f x) (f y)

instance Traversable Wye where
	End ->> f = point End
	Left x ->> f = Left <$> f x
	Right y ->> f = Right <$> f y
	Both x y ->> f = Both <$> f x <*> f y
