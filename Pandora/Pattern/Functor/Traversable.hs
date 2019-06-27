module Pandora.Pattern.Functor.Traversable (Traversable (..)) where

import Pandora.Core.Functor (type (:.:))
import Pandora.Core.Morphism (identity, (.))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Applicative (Applicative)
import Pandora.Pattern.Functor.Pointable (Pointable)

{- |
> Let f :: (Applicative t, Applicative g) => t a -> u a
> Let p :: (Pointable t, Pointable g) => t a -> u a

> When providing a new instance, you should ensure it satisfies the four laws:
> * Naturality of traversing: g . traverse f ≡ traverse (g . f)
> * Naturality of sequencing: f . sequence = sequence . comap f
> * Preserving point: p (point x) ≡ point x
> * Preserving apply: f (x <*> y) ≡ f x <*> f y
-}

infixl 5 ->>, ->>>, ->>>>, ->>>>>

class Covariant t => Traversable t where
	{-# MINIMAL (->>) #-}
	-- | Infix version of 'traverse'
	(->>) :: (Pointable u, Applicative u) => t a -> (a -> u b) -> (u :.: t) b

	-- | Prefix version of '->>'
	traverse :: (Pointable u, Applicative u) => (a -> u b) -> t a -> (u :.: t) b
	traverse f t = t ->> f
	-- | The dual of 'distribute'
	sequence :: (Pointable u, Applicative u) => (t :.: u) a -> (u :.: t) a
	sequence t = t ->> identity
	(->>>) :: (Pointable u, Applicative u, Traversable v)
		=> (a -> u b) -> (v :.: t) a -> (u :.: v :.: t) b
	(->>>) = traverse . traverse
	(->>>>) :: (Pointable u, Applicative u, Traversable v, Traversable w)
		=> (a -> u b) -> (w :.: v :.: t) a -> (u :.: w :.: v :.: t) b
	(->>>>) = traverse . traverse . traverse
	(->>>>>) :: (Pointable u, Applicative u, Traversable v, Traversable w, Traversable j)
		=> (a -> u b) -> (j :.: w :.: v :.: t) a -> (u :.: j :.: w :.: v :.: t) b
	(->>>>>) = traverse . traverse . traverse . traverse
