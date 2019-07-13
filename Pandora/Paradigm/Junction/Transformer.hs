module Pandora.Paradigm.Junction.Transformer (Transformer (..), type (:>)) where

import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Pointable (Pointable)

class Transformer t where
	{-# MINIMAL lay, equip #-}
	type Layout (t :: * -> *) (u :: * -> *) (a :: *) = r | r -> t u
	lay :: Covariant u => u a -> Layout t u a
	equip :: Pointable u => t a -> Layout t u a

infixr 1 :>
type (:>) t u a = Transformer t => Layout t u a
