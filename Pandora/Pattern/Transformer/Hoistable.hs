module Pandora.Pattern.Transformer.Hoistable (Hoistable (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Functor.Covariant (Covariant_)

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Identity morphism: hoist identity ≡ identity
> * Interpreted of morphisms: hoist (f . g) ≡ hoist f . hoist g
-}

infixr 5 /|\

class Hoistable t where
	{-# MINIMAL (/|\) #-}
	(/|\) :: (Covariant_ u (->) (->)) => u ~> v -> t u ~> t v

	hoist :: (Covariant_ u (->) (->)) => u ~> v -> t u ~> t v
	hoist = (/|\)
