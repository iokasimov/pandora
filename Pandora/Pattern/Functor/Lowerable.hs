module Pandora.Pattern.Functor.Lowerable (Lowerable (..)) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Pattern.Functor.Covariant (Covariant)

class Lowerable t where
	lower :: Covariant u => t u ~> u
