module Pandora.Pattern.Functor.Monad where

import Pandora.Pattern.Morphism.Straight (Straight)
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Bindable (Bindable)
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Operation.Product ((:*:))

{- |
> Let f :: (Monoidal t (->) (->) (:*:) (:*:), Bindable t) => a -> t a
> Let g :: (Monoidal t (->) (->) (:*:) (:*:), Bindable t) => a -> t a
> Let h :: (Monoidal t (->) (->) (:*:) (:*:), Bindable t) => t a

> When providing a new instance, you should ensure it satisfies:
> * Left identity: point a >>= f ≡ f a
> * Right identity: h >>= point ≡ h
> * Associativity: h >>= (f >=> g) ≡ (h >>= f) >>= g
-}

--infixl 1 >>=-, ->>=
--infixr 1 -=<<, =<-/-

class (Covariant category category t, Monoidal (Straight category) (Straight category) (:*:) (:*:) t, Bindable category t) => Monad category t where
	--(>>=-) :: t a -> t b -> t a
	--(>>=-) x y = x >>= \r -> y >>= \_ -> point r
	--(->>=) :: t a -> t b -> t b
	--(->>=) x y = x >>= \_ -> y >>= \r -> point r
	--(-=<<) :: t a -> t b -> t b
	--(-=<<) x y = x >>= \_ -> y >>= \r -> point r
	--(=<-/-) :: t a -> t b -> t a
	--(=<-/-) x y = x >>= \r -> y >>= \_ -> point r
