module Pandora.Core.Appliable where

class Appliable m a b n c d where
	(!) :: m a b -> n c d
