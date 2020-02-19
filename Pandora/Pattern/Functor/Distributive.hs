module Pandora.Pattern.Functor.Distributive (Distributive (..)) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Core.Morphism (identity, (.), (%))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))

{- |
> Let f :: Distributive g => (a -> g b)

> When providing a new instance, you should ensure it satisfies the two laws:
> * Identity morphism: distribute . distribute ≡ identity
> * Interchange collection: collect f ≡ distribute . comap f
-}

infixl 5 >>-, >>>-, >>>>-, >>>>>-

class Covariant u => Distributive u where
	{-# MINIMAL (>>-) #-}
	-- | Infix and flipped version of 'collect'
	(>>-) :: Covariant t => t a -> (a -> u b) -> u :. t > b

	-- | Prefix version of '>>-'
	collect :: Covariant t => (a -> u b) -> t a -> u :. t > b
	collect f t = t >>- f
	-- | The dual of 'sequence'
	distribute :: Covariant t => t :. u > a -> u :. t > a
	distribute t = t >>- identity

	-- | Infix versions of `collect` with various nesting levels
	(>>>-) :: (Covariant t, Covariant v)
		=> t :. v > a -> (a -> u b) -> u :. t :. v > b
	x >>>- f = (collect . collect) f x
	(>>>>-) :: (Covariant t, Covariant v, Covariant w)
		=> t :. v :. w > a -> (a -> u b) -> u :. t :. v :. w > b
	x >>>>- f = (collect . collect . collect) f x
	(>>>>>-) :: (Covariant t, Covariant v, Covariant w, Covariant j)
		=> t :. v :. w :. j > a -> (a -> u b) -> u :. t :. v :. w :. j > b
	x >>>>>- f = (collect . collect . collect . collect) f x

instance Distributive ((->) e) where
	g >>- f = \e -> (f % e) <$> g
