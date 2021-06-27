module Pandora.Pattern.Functor.Extendable where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_)

infixl 1 =>>
infixr 1 <<=, =<=, =>=

{- |
> When providing a new instance, you should ensure it satisfies:
> * Duplication interchange: comap (comap f) . duplicate ≡ duplicate . comap f
> * Extension interchange: extend f ≡ comap f . duplicate
-}

class Covariant t => Extendable t where
	{-# MINIMAL (=>>) #-}
	-- | Infix and flipped version of 'extend', the dual of '>>='
	(=>>) :: t a -> (t a -> b) -> t b

	-- | Flipped version of '>>=', the dual of '=<<'
	(<<=) :: (t a -> b) -> t a -> t b
	f <<= x = x =>> f
	-- | Prefix and flipped version of '=>>', the dual of 'bind'
	extend :: (t a -> b) -> t a -> t b
	extend f t = t =>> f
	-- | Clone existing structure, the dual of 'join'
	duplicate :: t a -> t :. t := a
	duplicate t = t =>> (\x -> x)
	-- | Right-to-left Cokleisli composition
	(=<=) :: (t b -> c) -> (t a -> b) -> t a -> c
	f =<= g = \x -> f (extend g x)

	-- | Left-to-right Cokleisli composition
	(=>=) :: (t a -> b) -> (t b -> c) -> t a -> c
	f =>= g = \x -> g (extend f x)

	-- | Experimental methods
	($=>>) :: Covariant u => u :. t := a -> (t a -> b) -> u :. t := b
	x $=>> f = (=>> f) <$> x
	(<<=$) :: Covariant u => u :. t := a -> (t a -> b) -> u :. t := b
	x <<=$ f = (=>> f) <$> x

class Covariant_ t source source => Extendable_ t source where
	duplicate_ :: source (t a) (t (t a))
