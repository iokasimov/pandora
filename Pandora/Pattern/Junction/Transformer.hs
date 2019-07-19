module Pandora.Pattern.Junction.Transformer (Transformer (..), type (:>)) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Pattern.Junction.Composition (Composition)
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Pointable (Pointable)

class Composition t => Transformer t where
	{-# MINIMAL lay, wrap #-}
	type Schema (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
	lay :: Covariant u => u ~> Schema t u
	wrap :: Pointable u => t ~> Schema t u

infixr 1 :>
type (:>) t u a = Transformer t => Schema t u a