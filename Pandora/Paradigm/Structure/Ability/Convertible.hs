{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Convertible where

import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Convertible f t u a where
	conversion :: Tagged f (t a) -> u a

convert :: forall f t u a . Convertible f t u a => t a -> u a
convert = conversion . Tag @f
