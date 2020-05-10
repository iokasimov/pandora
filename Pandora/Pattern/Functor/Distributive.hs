module Pandora.Pattern.Functor.Distributive (Distributive (..)) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Morphism ((%))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Category (identity, (.))

{- |
> Let f :: Distributive g => (a -> g b)

> When providing a new instance, you should ensure it satisfies the two laws:
> * Identity morphism: distribute . distribute ≡ identity
> * Interchange collection: collect f ≡ distribute . comap f
-}

infixl 5 >>-, >>>-, >>>>-, >>>>>-

class Covariant t => Distributive t where
	{-# MINIMAL (>>-) #-}
	-- | Infix and flipped version of 'collect'
	(>>-) :: Covariant u => u a -> (a -> t b) -> t :. u := b

	-- | Prefix version of '>>-'
	collect :: Covariant u => (a -> t b) -> u a -> t :. u := b
	collect f t = t >>- f
	-- | The dual of 'sequence'
	distribute :: Covariant u => u :. t := a -> t :. u := a
	distribute t = t >>- identity

	-- | Infix versions of `collect` with various nesting levels
	(>>>-) :: (Covariant u, Covariant v)
		=> u :. v := a -> (a -> t b) -> t :. u :. v := b
	x >>>- f = (collect . collect) f x
	(>>>>-) :: (Covariant u, Covariant v, Covariant w)
		=> u :. v :. w := a -> (a -> t b) -> t :. u :. v :. w := b
	x >>>>- f = (collect . collect . collect) f x
	(>>>>>-) :: (Covariant u, Covariant v, Covariant w, Covariant j)
		=> u :. v :. w :. j := a -> (a -> t b) -> t :. u :. v :. w :. j := b
	x >>>>>- f = (collect . collect . collect . collect) f x

instance Distributive ((->) e) where
	g >>- f = \e -> (f % e) <$> g
