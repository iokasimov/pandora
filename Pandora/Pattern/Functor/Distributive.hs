module Pandora.Pattern.Functor.Distributive where

import Pandora.Pattern.Functor.Covariant (Covariant)

{- |
> Let f :: Distributive g => (a -> g b)

> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: (identity -<<) . (identity -<<) ≡ identity
> * Interchange collection: (f -<<) ≡ (identity -<<) . (f <$>)
-}

infixl 5 -<<

class Covariant source target t => Distributive source target t where
	(-<<) :: Covariant source target u => source a (t b) -> target (u a) (t (u b))
