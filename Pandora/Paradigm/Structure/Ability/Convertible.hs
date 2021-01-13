{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Convertible where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Convertible f t u where
	conversion :: Tagged f (t a) -> u a

convert :: forall f t u . Convertible f t u => t ~> u
convert = conversion . Tag @f
