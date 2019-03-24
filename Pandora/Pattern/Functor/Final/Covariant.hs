module Pandora.Pattern.Functor.Final.Covariant (Cofinal (..)) where

class Cofinal (t :: * -> *) where
	{-# MINIMAL (<.$.>) #-}
	-- | Infix version of 'cofinal'
	(<.$.>) :: (a -> b) -> t a -> t b

	-- | Prefix version of '<.$.>'
	cofinal :: (a -> b) -> t a -> t b
	cofinal f x = f <.$.> x
