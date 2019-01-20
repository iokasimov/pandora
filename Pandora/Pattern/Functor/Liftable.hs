module Pandora.Pattern.Functor.Liftable (Liftable (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

class Liftable t where
	lift :: Covariant u => u a -> t u a
