module Pandora.Pattern.Functor.Invariant where

{- |
> When providing a new instance, you should ensure it satisfies:
> Identity morphisms: invmap identity identity = identity
> Interpreted of morphisms: invmap g j . invmap f h = invmap (g . f) (h . j)
-}

class Invariant (t :: * -> *) where
	{-# MINIMAL (>-<) #-}
	(>-<) :: (a -> b) -> (b -> a) -> t a -> t b
	-- | Prefix version of '>-<'
	invmap :: (a -> b) -> (b -> a) -> t a -> t b
	invmap f g x = (f >-< g) x
