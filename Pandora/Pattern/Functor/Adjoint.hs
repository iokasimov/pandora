module Pandora.Pattern.Functor.Adjoint (Adjoint (..), type (-|)) where

import Pandora.Core.Composition ((:.:))
import Pandora.Core.Morphism (identity)
import Pandora.Pattern.Functor.Covariant (Covariant)

{- |
> When providing a new instance, you should ensure it satisfies the four laws:
> * Left adjunction identity: phi counit ≡ identity
> * Right adjunction identity: psi unit ≡ identity
> * Left adjunction interchange: phi f ≡ comap f . eta
> * Right adjunction interchange: psi f ≡ epsilon . comap f
-}

class (Covariant t, Covariant u) => Adjoint t u where
	{-# MINIMAL phi, psi #-}
	-- | Left adjunction
	phi :: (t a -> b) -> a -> u b
	-- | Right adjunction
	psi :: (a -> u b) -> t a -> b

	eta :: a -> (u :.: t) a
	eta = phi identity
	epsilon :: (t :.: u) a -> a
	epsilon = psi identity

type (-|) = Adjoint
