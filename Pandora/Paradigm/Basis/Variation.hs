module Pandora.Paradigm.Basis.Variation (Variation (..)) where

import Pandora.Core.Morphism (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))

data Variation e a = This a | That e | These a e

instance Covariant (Variation e) where
	f <$> This x = This $ f x
	f <$> That y = That y
	f <$> These x y = These (f x) y
