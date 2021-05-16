module Pandora.Pattern.Category (Category (..)) where

infixl 2 #
infixr 0 $
infixr 9 .

{- |
> When providing a new instance, you should ensure it satisfies:
> * Left identity: identity . f ≡ f
> * Right identity: f . identity ≡ f
> * Associativity: f . (g . h) ≡ (f . g) . h
-}

class Category (m :: * -> * -> *) where
	identity :: m a a
	(.) :: m b c -> m a b -> m a c

	($) :: m (m a b) (m a b)
	($) = identity . identity

	(#) :: m (m a b) (m a b)
	(#) = identity . identity
