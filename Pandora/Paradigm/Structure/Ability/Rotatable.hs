{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Rotatable where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

class Rotatable f t where
	type Rotational (f :: k) (t :: * -> *) :: * -> *
	rotation :: Tagged f <:.> t ~> Rotational f t

rotate :: forall f t . Rotatable f t => t ~> Rotational f t
rotate = rotation . TU . Tag @f
