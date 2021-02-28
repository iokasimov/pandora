module Pandora.Pattern.Functor.Bivariant where

infixl 4 <->

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Identity: bimap identity identity ≡ identity
> * Parametricity: bimap  (f . g) (h . i) ≡ bimap f h . bimap g i
-}

class Bivariant (v :: * -> * -> *) where
	{-# MINIMAL (<->) #-}
	(<->) :: (a -> b) -> (c -> d) -> v a c -> v b d
	-- | Prefix version of '<->'
	bimap :: (a -> b) -> (c -> d) -> v a c -> v b d
	bimap f g x = (f <-> g) x
