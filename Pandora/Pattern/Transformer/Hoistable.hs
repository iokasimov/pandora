module Pandora.Pattern.Transformer.Hoistable (Hoistable (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Exactly morphism: (identity /|\) ≡ identity
> * Interpreted of morphisms: (f . g /|\) ≡ (f /|\) . (g /|\)
-}

infixr 5 /|\

class Hoistable m t where
	{-# MINIMAL (/|\) #-}
	(/|\) :: Covariant m m u => (forall a . m (u a) (v a)) -> (forall a . m (t u a) (t v a))
