module Pandora.Pattern.Category (Category (..)) where

import Pandora.Core.Functor (type (~~>))

infixl 1 #
infixl 2 #:
infixl 3 #::
infixl 4 #:::
infixr 0 $
infixr 8 .

class Category (m :: * -> * -> *) where
	identity :: m a a
	(.) :: m b c -> m a b -> m a c

	($) :: m ~~> m
	($) f = identity . f

	(#) :: m ~~> m
	(#) f = identity . f

	(#:) :: m ~~> m
	(#:) f = identity . f

	(#::) :: m ~~> m
	(#::) f = identity . f

	(#:::) :: m ~~> m
	(#:::) f = identity . f
