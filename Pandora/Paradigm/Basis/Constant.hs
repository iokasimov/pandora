module Pandora.Paradigm.Basis.Constant (Constant (..)) where

import Pandora.Core.Morphism (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))

newtype Constant a b = Constant a

instance Covariant (Constant a) where
	_ <$> Constant x = Constant x

instance Contravariant (Constant a) where
	_ >$< Constant x = Constant x

instance Traversable (Constant a) where
	Constant x ->> _ = point (Constant x)
