module Pandora.Pattern.Transformer.Liftable (Liftable (..)) where

import Pandora.Core.Functor (type (~>))

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Interchange: lift . point ≡ point
-}

class Liftable t where
	lift :: u ~> t u
