module Pandora.Paradigm.Basis.Capture (Capture (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Invariant (Invariant (invmap))

newtype Capture r t a = Capture { captured :: t r }

instance Covariant (Capture r t) where
	_ <$> Capture x = Capture x

instance Contravariant (Capture r t) where
	_ >$< Capture x = Capture x

instance Invariant (Capture r t) where
	invmap _ _ (Capture x) = Capture x
