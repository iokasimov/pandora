module Pandora.Pattern.Functor.Adjoint (Adjoint (..), type (-|)) where

import Pandora.Core.Functor (type (:.:), type (><))
import Pandora.Core.Morphism (identity)
import Pandora.Pattern.Functor.Covariant (Covariant)

type (-|) = Adjoint

infixl 4 -|, |-

{- |
> When providing a new instance, you should ensure it satisfies the four laws:
> * Left adjunction identity: phi cozero ≡ identity
> * Right adjunction identity: psi zero ≡ identity
> * Left adjunction interchange: phi f ≡ comap f . eta
> * Right adjunction interchange: psi f ≡ epsilon . comap f
-}

class (Covariant t, Covariant u) => Adjoint t u where
	{-# MINIMAL (-|), (|-) #-}
	-- | Left adjunction
	(-|) :: a -> (t a -> b) -> u b
	-- | Right adjunction
	(|-) :: t a -> (a -> u b) -> b

	-- | Prefix and flipped version of '-|'
	phi :: (t a -> b) -> a -> u b
	phi f x = x -| f
	-- | Prefix and flipped version of '|-'
	psi :: (a -> u b) -> t a -> b
	psi g x = x |- g
	-- | Also known as 'unit'
	eta :: a -> u :.: t >< a
	eta = phi identity
	-- | Also known as 'counit'
	epsilon :: t :.: u >< a -> a
	epsilon = psi identity
