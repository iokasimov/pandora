module Pandora.Pattern.Functor.Monad where

import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Bindable (Bindable)
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))

{- |
> Let f :: (Pointable t, Bindable t) => a -> t a
> Let g :: (Pointable t, Bindable t) => a -> t a
> Let h :: (Pointable t, Bindable t) => t a

> When providing a new instance, you should ensure it satisfies:
> * Left identity: point a >>= f ≡ f a
> * Right identity: h >>= point ≡ h
> * Associativity: h >>= (f >=> g) ≡ (h >>= f) >>= g
-}

--infixl 1 >>=-, ->>=
--infixr 1 -=<<, =<<-

class (Covariant t (->) (->), Monoidal t (->) (->) (:*:) (:*:), Bindable t (->)) => Monad t where
	--(>>=-) :: t a -> t b -> t a
	--(>>=-) x y = x >>= \r -> y >>= \_ -> point r
	--(->>=) :: t a -> t b -> t b
	--(->>=) x y = x >>= \_ -> y >>= \r -> point r
	--(-=<<) :: t a -> t b -> t b
	--(-=<<) x y = x >>= \_ -> y >>= \r -> point r
	--(=<<-) :: t a -> t b -> t a
	--(=<<-) x y = x >>= \r -> y >>= \_ -> point r
