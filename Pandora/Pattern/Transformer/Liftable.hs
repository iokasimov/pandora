module Pandora.Pattern.Transformer.Liftable (Liftable (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant_)

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Interchange: lift . point ≡ point
-}

class Liftable t where
	lift :: (Covariant u, Covariant_ u (->) (->)) => u ~> t u
