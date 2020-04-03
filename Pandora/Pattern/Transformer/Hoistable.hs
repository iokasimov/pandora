module Pandora.Pattern.Transformer.Hoistable (Hoistable (..)) where

import Pandora.Core.Functor (type (~>))

{- |
> When providing a new instance, you should ensure it satisfies one law:
> * Identity morphism: hoist identity ≡ identity
> * Interpreted of morphisms: hoist (f . g) ≡ hoist f . hoist g
-}

class Hoistable t where
	hoist :: u ~> v -> t u ~> t v
