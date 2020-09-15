module Pandora.Pattern.Transformer.Hoistable (Hoistable (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Functor.Covariant (Covariant)

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Identity morphism: hoist identity ≡ identity
> * Interpreted of morphisms: hoist (f . g) ≡ hoist f . hoist g
-}

class Hoistable t where
	hoist :: Covariant u => u ~> v -> t u ~> t v

-- data family Schematron (c :: (* -> *) -> k) (t :: * -> *) (u :: * -> *) :: * -> *
--
-- instance Hoistable (Schematron c t) where
