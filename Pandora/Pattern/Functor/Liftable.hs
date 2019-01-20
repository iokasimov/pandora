module Pandora.Pattern.Functor.Liftable (Liftable (..)) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Pattern.Functor.Covariant (Covariant)

class Liftable t where
	lift :: Covariant u => u ~> t u
