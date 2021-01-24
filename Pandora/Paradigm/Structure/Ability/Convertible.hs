{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Convertible where

import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Convertible f u t where
	type Conversion (f :: k) (t :: * -> *) a
	conversion :: Tagged f (t a) -> Conversion f t a

convert :: forall f u t a . Convertible f u t => t a -> Conversion f t a
convert = conversion @f @u @t . Tag @f

data Walk a = Preorder a | Inorder a | Postorder a | Levelorder a
