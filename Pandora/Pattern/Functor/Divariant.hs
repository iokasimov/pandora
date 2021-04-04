module Pandora.Pattern.Functor.Divariant where

infixl 4 >->

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity: dimap identity identity ≡ identity
> * Interpreted: dimap (f . g) (h . i) ≡ dimap g h . dimap f i
-}

class Divariant (v :: * -> * -> *) where
	{-# MINIMAL (>->) #-}
	(>->) :: (a -> b) -> (c -> d) -> v b c -> v a d
	-- | Prefix version of '>->'
	dimap :: (a -> b) -> (c -> d) -> v b c -> v a d
	dimap f g x = (f >-> g) x
