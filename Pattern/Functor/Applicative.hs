module Pattern.Functor.Applicative (Applicative (..)) where

import Core.Morphism (identity)
import Pattern.Functor.Covariant (Covariant ((<$)))

infixl 4 <*>, <*, *>

{- |
> When providing a new instance, you should ensure it satisfies the three laws:
> * Composition: (.) <$> u <*> v <*> w ≡ u <*> (v <*> w)
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
