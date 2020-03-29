module Pandora.Pattern.Transformer.Lowerable (Lowerable (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Functor.Extractable (Extractable)

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Interchange: extract . lower ≡ extract
-}

class Lowerable t where
	lower :: Extractable u => t u ~> u
