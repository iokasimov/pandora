module Pandora.Pattern.Functor.Distributive where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant_)

{- |
> Let f :: Distributive g => (a -> g b)

> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: (identity --<<-) . (identity --<<-) ≡ identity
> * Interchange collection: (f --<<-) ≡ (identity --<<-) . (f -<$>-)
-}

infixl 5 --<<-

class Covariant_ t source target => Distributive t source target where
	(--<<-) :: Covariant_ u source target => source a (t b) -> target (u a) (t (u b))
