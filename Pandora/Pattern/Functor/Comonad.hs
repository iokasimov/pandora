module Pandora.Pattern.Functor.Comonad where

import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Functor.Extendable (Extendable)
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))

{- |
> Let f :: (Extendable t, Extractable t) => t a -> b
> Let g :: (Extendable t, Extractable t) => t a -> b

> When providing a new instance, you should ensure it satisfies:
> * Left identity: extend extract ≡ identity
> * Right identity: extract . extend f ≡ f
> * Associativity: extend f . extend g ≡ extend (f . extend g)
-}

class (Monoidal t (<--) source (:*:) (:*:), Extendable source t) => Comonad t source
