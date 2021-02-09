module Pandora.Pattern.Category (Category (..)) where

import Pandora.Core.Functor (type (~~>))

infixr 8 .
infixr 0 $

class Category (m :: * -> * -> *) where
	identity :: m a a
	(.) :: m b c -> m a b -> m a c

	($) :: m ~~> m
	($) f = identity . f

instance Category (->) where
	identity x = x
	f . g = \x -> f (g x)
