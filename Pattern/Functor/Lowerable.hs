module Pattern.Functor.Lowerable (Lowerable (..)) where

import Pattern.Functor.Covariant (Covariant)

class Lowerable t where
	lower :: Covariant u => t u a -> u a
