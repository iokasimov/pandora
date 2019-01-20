module Pandora.Pattern.Functor.Extractable (Extractable (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

class Covariant t => Extractable t where
	extract :: t a -> a
