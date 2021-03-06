module Pandora.Pattern.Functor.Bindable where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))

infixl 1 >>=
infixr 1 =<<, <=<, >=>

{- |
> When providing a new instance, you should ensure it satisfies :
> * Interchange: t >>= f = join (f <$> t)
-}

class Covariant t => Bindable t where
	{-# MINIMAL (>>=) #-}
	-- | Infix and flipped version of 'bind', the dual of '=>>'
	(>>=) :: t a -> (a -> t b) -> t b

	-- | Flipped version of '>>=', the dual of '<<='
	(=<<) :: (a -> t b) -> t a -> t b
	f =<< x = x >>= f
	-- | Prefix and flipped version of '>>=', the dual of 'extend'
	bind :: (a -> t b) -> t a -> t b
	bind f t = t >>= f
	-- | Merge effects/contexts, the dual of 'duplicate'
	join :: t :. t := a -> t a
	join t = t >>= \x -> x
	-- | Left-to-right Kleisli composition
	(>=>) :: (a -> t b) -> (b -> t c) -> (a -> t c)
	f >=> g = \x -> f x >>= g
	-- | Right-to-left Kleisli composition
	(<=<) :: (b -> t c) -> (a -> t b) -> (a -> t c)
	g <=< f = f >=> g

	($>>=) :: Covariant u => u :. t := a -> (a -> t b) -> u :. t := b
	x $>>= f = (>>= f) <$> x

class Covariant_ t source source => Bindable_ t source where
	join_ :: source (t (t a)) (t a)

	(-=<<-) :: source a (t b) -> source (t a) (t b)
	(-=<<-) f = join_ . (-<$>-) f
