module Pandora.Pattern.Functor.Traversable where

import Pandora.Pattern.Functor.Covariant (Covariant_)
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal)
import Pandora.Pattern.Functor.Pointable (Pointable)
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))

{- |
> Let f :: (Applicative t, Applicative g) => t a -> u a
> Let p :: (Pointable t, Pointable g) => t a -> u a

> When providing a new instance, you should ensure it satisfies:
> * Numeratority of traversing: g . (f <<--) ≡ (g . f <<--)
> * Numeratority of sequencing: f . (identity <<--)= (identity <<--) . (f -<$>-)
> * Preserving point: p (point x) ≡ point x
> * Preserving apply: f (x -<*>- y) ≡ f x -<*>- f y
-}

infixl 5 <<-, -<<-<<-

class Covariant_ t source target => Traversable t source target where
	(<<-) :: (Covariant_ u source target, Pointable u target, Semimonoidal u target (:*:) (:*:)) => source a (u b) -> target (t a) (u (t b))

(-<<-<<-) :: forall t u v category a b .
	(Traversable t category category, Covariant_ u category category, Pointable u category, Semimonoidal u category (:*:) (:*:), Traversable v category category)
	=> category a (u b) -> category (v (t a)) (u (v (t b)))
(-<<-<<-) f = ((<<-) ((<<-) @t @category @category f))
