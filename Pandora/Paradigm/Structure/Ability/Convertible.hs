{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Convertible where

import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Convertible f t where
	type Conversion (f :: k) (t :: * -> *) :: * -> *
	conversion :: Tagged f (t a) -> (Conversion f t) a

convert :: forall f t a . Convertible f t => t a -> (Conversion f t) a
convert = conversion @f @t . Tag @f

data Walk a = Preorder a | Inorder a | Postorder a | Levelorder a
