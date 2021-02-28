module Pandora.Pattern.Category (Category (..)) where

import Pandora.Core.Functor (type (~~>))

infixr 8 .
infixl 1 /
infixr 0 $

class Category (m :: * -> * -> *) where
	identity :: m a a
	(.) :: m b c -> m a b -> m a c

	($) :: m ~~> m
	($) f = identity . f

	(/) :: m ~~> m
	(/) f = identity . f
