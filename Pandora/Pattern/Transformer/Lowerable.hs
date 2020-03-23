module Pandora.Pattern.Transformer.Lowerable (Lowerable (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Functor.Covariant (Covariant)

class Lowerable t where
	lower :: Covariant u => t u ~> u
