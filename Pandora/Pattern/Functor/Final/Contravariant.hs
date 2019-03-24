module Pandora.Pattern.Functor.Final.Contravariant (Contrafinal (..)) where

class Contrafinal (t :: * -> *) where
	{-# MINIMAL (>.$.<) #-}
	-- | Infix version of 'contrafinal'
	(>.$.<) :: (a -> b) -> t a -> t b

	-- | Prefix version of '>.$.<'
	contrafinal :: (a -> b) -> t a -> t b
	contrafinal f x = f >.$.< x
