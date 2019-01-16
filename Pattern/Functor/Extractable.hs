module Pattern.Functor.Extractable (Extractable (..)) where

import Pattern.Functor.Covariant (Covariant)

class Covariant t => Extractable t where
	extract :: t a -> a
