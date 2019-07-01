module Pandora.Pattern.Object.Quasiring (Quasiring (..)) where

import Pandora.Pattern.Object.Monoid (Monoid)
import Pandora.Pattern.Object.Ringoid (Ringoid)

{- |
> When providing a new instance, you should ensure it satisfies the one law:
> * Additive identity is a multiplicative annihilator: zero * x = x  * zero = zero
-}

class (Monoid a, Ringoid a) => Quasiring a where
	one :: a
