module Pandora.Pattern.Category (Category (..)) where

infixr 8 .

class Category (m :: * -> * -> *) where
	identity :: m a a
	(.) :: m b c -> m a b -> m a c

instance Category (->) where
	identity x = x
	f . g = \x -> f (g x)
