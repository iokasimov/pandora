module Pandora.Pattern.Functor.Applicative (Applicative (..)) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Core.Morphism (identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$)))

infixl 4 <*>, <*, *>

{- |
> When providing a new instance, you should ensure it satisfies the three laws:
> * Interpreted: (.) <$> u <*> v <*> w ≡ u <*> (v <*> w)
> * Left interchange: x <*> (f <$> y) ≡ (. f) <$> x <*> y
> * Right interchange: f <$> (x <*> y) ≡ (f .) <$> x <*> y
-}

class Covariant t => Applicative t where
	{-# MINIMAL (<*>) #-}
	-- | Infix version of 'apply'
	(<*>) :: t (a -> b) -> t a -> t b

	-- | Prefix version of '<*>'
	apply :: t (a -> b) -> t a -> t b
	apply f x = f <*> x
	-- | Sequence actions, discarding the value of the first argument
	(*>) :: t a -> t b -> t b
	x *> y = (identity <$ x) <*> y
	-- | Sequence actions, discarding the value of the second argument
	(<*) :: t a -> t b -> t a
	x <* y = y *> x
	-- | Repeat an action indefinitely
	forever :: t a -> t b
	forever x = x *> forever x

	-- | Infix versions of `apply` with various nesting levels
	(<**>) :: Applicative u => t :. u > (a -> b) -> t :. u > a -> t :. u > b
	f <**> x = (<*>) <$> f <*> x
	(<***>) :: (Applicative u, Applicative v) => t :. u :. v > (a -> b)
		-> t :. u :. v > a -> t :. u :. v > b
	f <***> x = (<**>) <$> f <*> x
	(<****>) :: (Applicative u, Applicative v, Applicative w)
		=> t :. u :. v :. w > (a -> b)
		-> t :. u :. v :. w > a
		-> t :. u :. v :. w > b
	f <****> x = (<***>) <$> f <*> x

instance Applicative ((->) e) where
	(<*>) f g x = f x (g x)
