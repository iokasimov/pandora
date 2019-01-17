module Pattern.Functor.Liftable (Liftable (..)) where

import Pattern.Functor.Covariant (Covariant)

class Liftable t where
	lift :: Covariant u => u a -> t u a
