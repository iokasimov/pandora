module Pandora.Pattern.Functor.Representable where

import Pandora.Core.Functor (type (<:=))

{- |
> When providing a new instance, you should ensure it satisfies:
> * Isomorphism (to): tabulate . index ≡ identity
> * Isomorphism (from): index . tabulate ≡ identity
> * Right adjoint: tabulate . point ≡ point
> * Interchange tabulation: comap f . tabulate ≡ tabulate . comap f
-}

infixr 7 <#>

class Representable t where
	{-# MINIMAL (<#>), tabulate #-}
	type Representation t :: *
	-- | Infix and flipped version of 'index'
	(<#>) :: Representation t -> a <:= t
	-- Build with a function which describes value
	tabulate :: (Representation t -> a) -> t a
	-- | Prefix and flipped version of '<#>'
	index :: t a -> Representation t -> a
	index x r = r <#> x
