module Pandora.Pattern.Functor.Contravariant where

import Pandora.Pattern.Category (Category)

infixl 4 ->$<-

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: (identity ->$<-) ≡ identity
> * Interpreted of morphisms: (f ->$<-) . (g ->$<-) ≡ (g . f ->$<-)
-}

class (Category source, Category target) => Contravariant t source target where
	(->$<-) :: source a b -> target (t b) (t a)
