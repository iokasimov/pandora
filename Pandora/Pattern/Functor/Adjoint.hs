module Pandora.Pattern.Functor.Adjoint where

import Pandora.Pattern.Functor.Covariant (Covariant)

type (-|) = Adjoint

infixl 1 --------|, |--------
infixl 2 -------|, |-------
infixl 3 ------|, |------
infixl 4 -----|, |-----
infixl 5 ----|, |----
infixl 6 ---|, |---
infixl 7 --|, |--
infixl 8 -|, |-

{- |
> When providing a new instance, you should ensure it satisfies:
> * Left adjunction identity: phi cozero ≡ identity
> * Right adjunction identity: psi zero ≡ identity
> * Left adjunction interchange: phi f ≡ comap f . eta
> * Right adjunction interchange: psi f ≡ epsilon . comap f
-}

class (Covariant target source t, Covariant source target u) => Adjoint source target t u where
	(-|) :: source (t a) b -> target a (u b)
	(|-) :: target a (u b) -> source (t a) b

	(|--------), (|-------), (|------),  (|-----),  (|----), (|---), (|--) :: target a (u b) -> source (t a) b
	(|--------) = (|-)
	(|-------) = (|-)
	(|------) = (|-)
	(|-----) = (|-)
	(|----) = (|-)
	(|---) = (|-)
	(|--) = (|-)

	(--------|), (-------|), (------|), (-----|), (----|), (---|), (--|) :: source (t a) b -> target a (u b)
	(--------|) = (-|)
	(-------|) = (-|)
	(------|) = (-|)
	(-----|) = (-|)
	(----|) = (-|)
	(---|) = (-|)
	(--|) = (-|)
