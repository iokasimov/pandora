module Pandora.Pattern.Functor.Distributive (Distributive (..)) where

import Pandora.Core.Functor (type (:.:))
import Pandora.Core.Morphism (identity)
import Pandora.Pattern.Functor.Covariant (Covariant)

{- |
> Let f :: Distributive g => (a -> g b)

> When providing a new instance, you should ensure it satisfies the two laws:
> * Identity morphism: distribute . distribute ≡ identity
> * Interchange collection: collect f ≡ distribute . comap f
-}

class Covariant u => Distributive u where
	{-# MINIMAL (>>-) #-}
	-- | Infix version of 'collect'
	(>>-) :: Covariant t => t a -> (a -> u b) -> (u :.: t) b

	-- | Prefix version of '>>-'
	collect :: Covariant t => (a -> u b) -> t a -> (u :.: t) b
	collect f t = t >>- f
	-- | The dual of 'sequence'
	distribute :: Covariant t => (t :.: u) a -> (u :.: t) a
	distribute t = t >>- identity
