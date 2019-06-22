module Pandora.Pattern.Functor.Avoidable (Avoidable (..)) where

import Pandora.Pattern.Functor.Alternative (Alternative)

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Left absorption: x <+> idle ≡ x
> * Right absorption: idle <+> x ≡ x
-}

class Alternative t => Avoidable t where
	idle :: t a
