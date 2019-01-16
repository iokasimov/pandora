module Pattern.Functor.Pointable (Pointable (..)) where

import Pattern.Functor.Covariant (Covariant)

class Covariant t => Pointable t where
	point :: a -> t a
