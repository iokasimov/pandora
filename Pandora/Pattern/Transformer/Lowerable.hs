module Pandora.Pattern.Transformer.Lowerable (Lowerable (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Interchange: extract . lower ≡ extract
-}

class Lowerable cat t where
	lower :: Covariant cat cat u => cat (t u a) (u a)
