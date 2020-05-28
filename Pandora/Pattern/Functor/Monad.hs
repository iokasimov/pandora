module Pandora.Pattern.Functor.Monad where

import Pandora.Pattern.Functor.Bindable (Bindable)
import Pandora.Pattern.Functor.Pointable (Pointable)

{- |
> Let f :: (Pointable t, Bindable t) => a -> t a
> Let g :: (Pointable t, Bindable t) => a -> t a
> Let h :: (Pointable t, Bindable t) => t a

> When providing a new instance, you should ensure it satisfies the three laws:
> * Left identity: point a >>= f ≡ f a
> * Right identity: h >>= point ≡ h
> * Associativity: h >>= (\x -> f x >>= g) ≡ (h >>= f) >>= g
-}

class (Pointable t, Bindable t) => Monad t
