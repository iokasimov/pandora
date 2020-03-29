module Pandora.Pattern.Transformer.Liftable (Liftable (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Functor.Pointable (Pointable)

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Interchange: lift . point ≡ point
-}

class Liftable t where
	lift :: Pointable u => u ~> t u
