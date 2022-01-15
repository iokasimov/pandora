module Pandora.Pattern.Category (Category (..)) where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))

infixl 2 #
infixl 8 <---
infixr 7 --->

{- |
> When providing a new instance, you should ensure it satisfies:
> * Left identity: identity . f ≡ f
> * Right identity: f . identity ≡ f
-}

class Semigroupoid m => Category m where
	identity :: m a a

	(#) :: m (m a b) (m a b)
	(#) = identity . identity

	(<---) :: m (m a b) (m a b)
	(<---) = identity . identity

	(--->) :: m (m a b) (m a b)
	(--->) = identity . identity
