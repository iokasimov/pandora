module Pandora.Pattern.Functor.Traversable where

import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))

{- |
> Let f :: (Applicative t, Applicative g) => t a -> u a
> Let p :: (Monoidal u category category (:*:) (:*:), Monoidal u category category (:*:) (:*:)) => t a -> u a

> When providing a new instance, you should ensure it satisfies:
> * Numeratority of traversing: g . (f <<--) ≡ (g . f <<--)
> * Numeratority of sequencing: f . (identity <<--)= (identity <<--) . (f -<$>-)
> * Preserving point: p (point x) ≡ point x
> * Preserving apply: f (x -<*>- y) ≡ f x -<*>- f y
-}

infixl 5 <<-, -<<-<<-

class Covariant source target t => Traversable t source target where
	(<<-) :: (Covariant source target u, Monoidal u source target (:*:) (:*:)) => source a (u b) -> target (t a) (u (t b))

(-<<-<<-) :: forall t u v category a b .
	(Traversable t category category, Covariant category category u, Monoidal u category category (:*:) (:*:), Traversable v category category)
	=> category a (u b) -> category (v (t a)) (u (v (t b)))
(-<<-<<-) f = ((<<-) ((<<-) @t @category @category f))
