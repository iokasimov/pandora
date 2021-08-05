module Pandora.Pattern.Functor.Contravariant where

import Pandora.Pattern.Category (Category)

infixl 4 ->$<-

-- TODO: use ->$<- instead of contramap here
{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: contramap identity ≡ identity
> * Interpreted of morphisms: contramap f . contramap g ≡ contramap (g . f)
-}

class (Category source, Category target) => Contravariant t source target where
	(->$<-) :: source a b -> target (t b) (t a)
