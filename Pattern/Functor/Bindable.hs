module Pattern.Functor.Bindable (Bindable (..)) where

import Core.Composition ((:.:))
import Core.Morphism (($), flip, identity)
import Pattern.Functor.Covariant (Covariant ((<$>)))

infixl 1 >>=
infixr 1 =<<, <=<, >=>

{- |
> When providing a new instance, you should ensure it satisfies the one law:
> * Interchange: t >>= f = join (f <$> t)
-}

class Covariant t => Bindable t where
	{-# MINIMAL (>>=) #-}
	-- | Infix and flipped version of 'bind', the dual of '=>>'
	(>>=) :: t a -> (a -> t b) -> t b

	-- | Flipped version of '>>=', the dual of '<<='
	(=<<) :: (a -> t b) -> t a -> t b
	(=<<) = flip (>>=)
	-- | Prefix and flipped version of '>>=', the dual of 'extend'
	bind :: (a -> t b) -> t a -> t b
	bind f t = t >>= f
	-- | Merge effects/contexts, the dual of 'duplicate'
	join :: (t :.: t) a -> t a
	join t = t >>= identity
	-- | Left-to-right Kleisli composition
	(>=>) :: (a -> t b) -> (b -> t c) -> (a -> t c)
	f >=> g = \x -> f x >>= g
	-- | Right-to-left Kleisli composition
	(<=<) :: (b -> t c) -> (a -> t b) -> (a -> t c)
	(<=<) = flip (>=>)
