module Pandora.Pattern.Functor.Comonad where

import Pandora.Pattern.Functor.Extractable (Extractable)
import Pandora.Pattern.Functor.Extendable (Extendable)

{- |
> Let f :: (Pointable t, Bindable t) => t a -> b
> Let g :: (Pointable t, Bindable t) => t a -> b

> When providing a new instance, you should ensure it satisfies:
> * Left identity: extend extract ≡ identity
> * Right identity: extract . extend f ≡ f
> * Associativity: extend f . extend g ≡ extend (f . extend g)
-}

class (Extractable t source, Extendable t (->)) => Comonad t source
