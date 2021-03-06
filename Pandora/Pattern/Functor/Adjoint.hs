module Pandora.Pattern.Functor.Adjoint where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>), (<$$$>), (<$$$$>)), Covariant_)

type (-|) = Adjoint

infixl 3 -|, |-, -|$, $|-, $$|-, $$$|-, $$$$|-

{- |
> When providing a new instance, you should ensure it satisfies:
> * Left adjunction identity: phi cozero ≡ identity
> * Right adjunction identity: psi zero ≡ identity
> * Left adjunction interchange: phi f ≡ comap f . eta
> * Right adjunction interchange: psi f ≡ epsilon . comap f
-}

class Adjoint t u where
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
	eta :: a -> u :. t := a
	eta = phi (\x -> x)
	-- | Also known as 'counit'
	epsilon :: t :. u := a -> a
	epsilon = psi (\x -> x)

	(-|$) :: Covariant v => v a -> (t a -> b) -> v (u b)
	x -|$ f = (-| f) <$> x

	-- | Versions of `|-` with various nesting levels
	($|-) :: Covariant v => v (t a) -> (a -> u b) -> v b
	x $|- f = (|- f) <$> x
	($$|-) :: (Covariant v, Covariant w) =>
		v :. w :. t := a -> (a -> u b) -> v :. w := b
	x $$|- f = (|- f) <$$> x
	($$$|-) :: (Covariant v, Covariant w, Covariant x) =>
		v :. w :. x :. t := a -> (a -> u b) -> v :. w :. x := b
	x $$$|- f = (|- f) <$$$> x
	($$$$|-) :: (Covariant v, Covariant w, Covariant x, Covariant y) =>
		v :. w :. x :. y :. t := a -> (a -> u b) -> v :. w :. x :. y := b
	x $$$$|- f = (|- f) <$$$$> x

class (Covariant_ t target source, Covariant_ u source target) => Adjoint_ t u source target where
	(--|-) :: source (t a) b -> target a (u b)
	(-|--) :: target a (u b) -> source (t a) b
