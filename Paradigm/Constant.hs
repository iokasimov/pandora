module Paradigm.Constant (Constant (..)) where

import Core.Morphism (($))
import Pattern.Functor.Covariant (Covariant ((<$>)))

newtype Constant a b = Constant a

instance Covariant (Constant a) where
	_ <$> Constant x = Constant x
