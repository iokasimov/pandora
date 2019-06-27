module Pandora.Pattern.Functor.Comonad (Comonad) where

import Pandora.Pattern.Functor.Extractable (Extractable)
import Pandora.Pattern.Functor.Extendable (Extendable)

{- |
> Let f :: (Pointable t, Bindable t) => t a -> b
> Let g :: (Pointable t, Bindable t) => t a -> b

> When providing a new instance, you should ensure it satisfies the three laws:
> * Left identity: extend extract ≡ identity
> * Right identity: extract . extend f ≡ f
> * Associativity: extend f . extend g ≡ extend (f . extend g)
-}

class (Extractable t, Extendable t) => Comonad t
