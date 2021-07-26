module Pandora.Pattern.Semigroupoid where

infixr 9 .

{- |
> When providing a new instance, you should ensure it satisfies:
> * Associativity: f . (g . h) ≡ (f . g) . h
-}

class Semigroupoid (m :: * -> * -> *) where
	(.) :: m b c -> m a b -> m a c
