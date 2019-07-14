module Pandora.Paradigm.Junction.Transformer (Transformer (..), type (:>)) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Pointable (Pointable)

class Transformer t where
	{-# MINIMAL lay, equip #-}
	type Layout (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
	lay :: Covariant u => u ~> Layout t u
	equip :: Pointable u => t ~> Layout t u

infixr 1 :>
type (:>) t u a = Transformer t => Layout t u a
