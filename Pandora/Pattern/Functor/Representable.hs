module Pandora.Pattern.Functor.Representable (Representable (..)) where

import Pandora.Pattern.Functor.Pointable (Pointable)

{- |
> When providing a new instance, you should ensure it satisfies the three laws:
> * Isomorphism (to): tabulate . index ≡ identity
> * Isomorphism (from): index . tabulate ≡ identity
> * Right adjoint: tabulate . point ≡ point
> * Interchange tabulation: comap f . tabulate ≡ tabulate . comap f
-}

class Pointable t => Representable t where
	{-# MINIMAL (<#>), tabulate #-}
	type Representation t :: *
	-- | Infix and flipped version of 'index'
	(<#>) :: Representation t -> t a -> a
	-- Build with a function which describes value
	tabulate :: (Representation t -> a) -> t a
	-- | Prefix version of '<#>'
	index :: t a -> Representation t -> a
