{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Rotatable where

import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Rotatable f t where
	type Rotational (f :: k) (t :: * -> *) a
	rotation :: Tagged f (t a) -> Rotational f t a

rotate :: forall f t a . Rotatable f t => t a -> Rotational f t a
rotate = rotation . Tag @f
