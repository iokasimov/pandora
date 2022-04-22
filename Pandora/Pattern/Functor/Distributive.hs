module Pandora.Pattern.Functor.Distributive where

import Pandora.Pattern.Functor.Covariant (Covariant)

{- |
> Let f :: Distributive g => (a -> g b)

> When providing a new instance, you should ensure it satisfies:
> * Exactly morphism: (identity -<<) . (identity -<<) ≡ identity
> * Interchange collection: (f -<<) ≡ (identity -<<) . (f <-|-)
-}

infixl 1 -------<<
infixl 2 ------<<
infixl 3 -----<<
infixl 4 ----<<
infixl 5 ---<<
infixl 6 --<<
infixl 7 -<<

class Covariant source target t => Distributive source target t where
	-- TODO: rename to <-\-
	(-<<) :: Covariant source target u => source a (t b) -> target (u a) (t (u b))

	(--<<), (---<<), (----<<), (-----<<), (------<<), (-------<<) :: Covariant source target u => source a (t b) -> target (u a) (t (u b))
	(--<<) = (-<<)
	(---<<) = (-<<)
	(----<<) = (-<<)
	(-----<<) = (-<<)
	(------<<) = (-<<)
	(-------<<) = (-<<)
