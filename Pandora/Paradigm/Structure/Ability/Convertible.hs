{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Convertible where

import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Convertible f u t where
	conversion :: Tagged f (t a) -> u a

convert :: forall f u t a . Convertible f u t => t a -> u a
convert = conversion . Tag @f

data Walk a = Preorder a | Inorder a | Postorder a | Levelorder a
