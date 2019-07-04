module Pandora.Pattern.Functor.Determinable (Determinable (..)) where

import Pandora.Pattern.Functor.Contravariant (Contravariant)

class Contravariant t => Determinable t where
	determine :: t a
