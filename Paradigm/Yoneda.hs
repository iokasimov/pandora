module Paradigm.Yoneda (Yoneda (..)) where

import Pattern.Morphism ((.))
import Pattern.Functor.Covariant (Covariant ((<$>)))

newtype Yoneda t a = Yoneda
	{ yoneda :: forall b . (a -> b) -> t b }

instance Covariant (Yoneda t) where
	f <$> x = Yoneda (\k -> yoneda x (k . f))
