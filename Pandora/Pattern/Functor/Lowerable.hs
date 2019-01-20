module Pandora.Pattern.Functor.Lowerable (Lowerable (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

class Lowerable t where
	lower :: Covariant u => t u a -> u a
