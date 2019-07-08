module Pandora.Paradigm.Junction.Transformer (Transformer (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

class Transformer o where
	type Layout (o :: * -> *) (i :: * -> *) (a :: *) = r | r -> o
	transformer :: Covariant i => i a -> Layout o i a
