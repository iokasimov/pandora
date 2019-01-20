module Pandora.Pattern.Functor.Pointable (Pointable (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

class Covariant t => Pointable t where
	point :: a -> t a
