module Pandora.Pattern.Category (Category (..)) where

infixr 8 .
infixr 0 $

class Category (m :: * -> * -> *) where
	identity :: m a a
	(.) :: m b c -> m a b -> m a c

	($) :: m a b -> m a b
	($) f = identity . f

instance Category (->) where
	identity x = x
	f . g = \x -> f (g x)
