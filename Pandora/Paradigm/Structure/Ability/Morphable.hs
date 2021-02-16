{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Morphable where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

class Morphable f t where
	type Morphing (f :: k) (t :: * -> *) :: * -> *
	morphing :: Tagged f <:.> t ~> Morphing f t

morph :: forall f t . Morphable f t => t ~> Morphing f t
morph = morphing . TU . Tag @f

data Walk a = Preorder a | Inorder a | Postorder a | Levelorder a

data Morph a = Rotate a | Into a

rotate :: forall f t . Morphable (Rotate f) t => t ~> Morphing (Rotate f) t
rotate = morphing . TU . Tag @(Rotate f)

into :: forall f t . Morphable (Into f) t => t ~> Morphing (Into f) t
into = morphing . TU . Tag @(Into f)
