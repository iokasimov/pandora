module Pandora.Pattern.Object.Cycle (Cycle (..)) where

import Pandora.Pattern.Object.Chain (Chain)

{- |
> When providing a new instance, you should ensure it satisfies:
> * Transitivity: x ≡ previous (next x)
-}

-- | Strict ternary relation order
class Chain a => Cycle a where
	{-# MINIMAL previous, next #-}
	previous :: a -> a
	next :: a -> a
