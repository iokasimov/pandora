module Pandora.Pattern.Functor.Avoidable where

import Pandora.Pattern.Functor.Alternative (Alternative)

{- |
> When providing a new instance, you should ensure it satisfies:
> * Left absorption: x <+> empty ≡ x
> * Right absorption: empty <+> x ≡ x
-}

class Alternative t => Avoidable t where
	{-# MINIMAL empty #-}
	empty :: t a
