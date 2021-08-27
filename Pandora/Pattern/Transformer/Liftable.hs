module Pandora.Pattern.Transformer.Liftable (Liftable (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Interchange: lift . point ≡ point
-}

class Liftable cat t where
	lift :: Covariant cat cat u => cat (u a) (t u a)
