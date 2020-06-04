{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Rotatable where

import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Rotatable (f :: k) t where
	rotation :: (Tagged f) (t a) -> Maybe (t a)

rotate :: forall f t a . Rotatable f t => t a -> Maybe (t a)
rotate = rotation . Tag @f
