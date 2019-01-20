module Pandora.Pattern.Functor.Exclusive (Exclusive (..)) where

import Pandora.Pattern.Functor.Alternative (Alternative)

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Left absorption: x <+> exclusive ≡ x
> * Right absorption: exclusive <+> x ≡ x
-}

class Alternative t => Exclusive t where
	exclusive :: t a
