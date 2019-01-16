module Paradigm.Constant (Constant (..)) where

import Core.Morphism (($))
import Pattern.Functor.Covariant (Covariant ((<$>)))
import Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Traversable (Traversable ((->>)))

newtype Constant a b = Constant a

instance Covariant (Constant a) where
	_ <$> Constant x = Constant x

instance Contravariant (Constant a) where
	_ >$< Constant x = Constant x

instance Traversable (Constant a) where
	Constant x ->> _ = point (Constant x)
