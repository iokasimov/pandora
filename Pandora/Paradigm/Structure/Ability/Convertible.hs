{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Convertible where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

class Convertible f t where
	type Conversion (f :: k) (t :: * -> *) :: * -> *
	conversion :: Tagged f <:.> t ~> Conversion f t

convert :: forall f t . Convertible f t => t ~> Conversion f t
convert = conversion @f @t . TU . Tag @f

data Walk a = Preorder a | Inorder a | Postorder a | Levelorder a
