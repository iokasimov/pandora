{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Measurable where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Measurable f t where
	type Measural (f :: k) (t :: * -> *) a
	measurement :: Tagged f (t a) -> Measural f t a

measure :: forall f t a . Measurable f t => t a -> Measural f t a
measure = measurement . Tag @f

data Scale = Length | Heighth | Depth
