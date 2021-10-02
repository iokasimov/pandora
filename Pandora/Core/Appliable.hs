module Pandora.Core.Appliable where

infixr 0 !

class Appliable m a b n c d | m a b -> n c d where
	(!) :: m a b -> n c d
