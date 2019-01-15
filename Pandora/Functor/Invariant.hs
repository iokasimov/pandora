module Pandora.Functor.Invariant (Invariant (..)) where

import Pandora.Morphism (flip)

infixl 4 >$>
infixr 4 <$<

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> Identity morphisms: invmap identity identity = identity
> Composition of morphisms: invmap g j . invmap f h = invmap (g . f) (h . j)
-}

class Invariant (t :: * -> *) where
	{-# MINIMAL (<$<) #-}
	-- | Infix version of 'invmap'
	(<$<) :: (a -> b) -> (b -> a) -> t a -> t b

	-- | Prefix version of '<$<'
	invmap :: (a -> b) -> (b -> a) -> t a -> t b
	invmap f x = f <$< x
	-- | Flipped version of '<$<'
	(>$>) :: (b -> a) -> (a -> b) -> t a -> t b
	(>$>) = flip (<$<)
