module Pandora.Pattern.Functor.Distributive where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant_)

{- |
> Let f :: Distributive g => (a -> g b)

> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: distribute . distribute ≡ identity
> * Interchange collection: collect f ≡ distribute . comap f
-}

infixl 5 >>-, >>>-, >>>>-, >>>>>-

class Covariant_ t (->) (->) => Distributive t where
	{-# MINIMAL (>>-) #-}
	-- | Infix and flipped version of 'collect'
	(>>-) :: Covariant_ u (->) (->) => u a -> (a -> t b) -> t :. u := b

	-- | Prefix version of '>>-'
	collect :: Covariant_ u (->) (->) => (a -> t b) -> u a -> t :. u := b
	collect f t = t >>- f
	-- | The dual of 'sequence'
	distribute :: Covariant_ u (->) (->) => u :. t := a -> t :. u := a
	distribute t = t >>- (\x -> x)

	-- | Infix versions of `collect` with various nesting levels
	(>>>-) :: (Covariant_ u (->) (->), Covariant_ v (->) (->))
		=> u :. v := a -> (a -> t b) -> t :. u :. v := b
	x >>>- f = x >>- (>>- f)
	(>>>>-) :: (Covariant_ u (->) (->), Covariant_ v (->) (->), Covariant_ w (->) (->))
		=> u :. v :. w := a -> (a -> t b) -> t :. u :. v :. w := b
	x >>>>- f = x >>- (>>- (>>- f))
	(>>>>>-) :: (Covariant_ u (->) (->), Covariant_ v (->) (->), Covariant_ w (->) (->), Covariant_ j (->) (->))
		=> u :. v :. w :. j := a -> (a -> t b) -> t :. u :. v :. w :. j := b
	x >>>>>- f = x >>- (>>- (>>- (>>- f)))

class Covariant_ t source target => Distributive_ t source target where
	(--<<-) :: Covariant_ u source target => source a (t b) -> target (u a) (t (u b))
