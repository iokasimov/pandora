module Pandora.Pattern.Object.Semigroup (Semigroup (..)) where

{- |
> When providing a new instance, you should ensure it satisfies the one law:
> * Associativity: x <> (y <> z) ≡ (x <> y) <> z
-}

class Semigroup a where
	{-# MINIMAL (<>) #-}
	-- | Infix version of 'append'
	(<>) :: a -> a -> a
