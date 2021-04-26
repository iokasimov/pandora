module Pandora.Pattern.Category (Category (..)) where

infixl 2 #
infixr 0 $
infixr 9 .

class Category (m :: * -> * -> *) where
	identity :: m a a
	(.) :: m b c -> m a b -> m a c

	($) :: m (m a b) (m a b)
	($) = identity . identity

	(#) :: m (m a b) (m a b)
	(#) = identity . identity
