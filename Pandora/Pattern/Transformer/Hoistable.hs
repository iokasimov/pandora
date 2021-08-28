module Pandora.Pattern.Transformer.Hoistable (Hoistable (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Functor.Covariant (Covariant)

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Identity morphism: (identity /|\) ≡ identity
> * Interpreted of morphisms: (f . g /|\) ≡ (f /|\) . (g /|\)
-}

infixr 5 /|\

class Hoistable t where
	{-# MINIMAL (/|\) #-}
	(/|\) :: (Covariant (->) (->) u) => u ~> v -> t u ~> t v
