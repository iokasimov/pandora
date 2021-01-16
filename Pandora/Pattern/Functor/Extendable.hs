module Pandora.Pattern.Functor.Extendable where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category (identity, (.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))

infixl 1 =>>
infixr 1 <<=, =<=, =>=

{- |
> When providing a new instance, you should ensure it satisfies the three laws:
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
	duplicate t = t =>> identity
	-- | Right-to-left Cokleisli composition
	(=<=) :: (t b -> c) -> (t a -> b) -> t a -> c
	f =<= g = f . extend g
	-- | Left-to-right Cokleisli composition
	(=>=) :: (t a -> b) -> (t b -> c) -> t a -> c
	f =>= g = g . extend f

	-- | Experimental methods
	($=>>) :: Covariant u => u :. t := a -> (t a -> b) -> u :. t := b
	x $=>> f = (=>> f) <$> x
	(<<=$) :: Covariant u => u :. t := a -> (t a -> b) -> u :. t := b
	x <<=$ f = (=>> f) <$> x
