module Pandora.Pattern.Functor.Covariant (Covariant (..)) where

import Pandora.Core.Morphism ((.), (!), (?))

infixl 4 <$>, <$, $>

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Identity morphism: comap identity ≡ identity
> * Composition of morphisms: comap (f . g) ≡ comap f . comap g
-}

class Covariant (t :: * -> *) where
	{-# MINIMAL (<$>) #-}
	-- | Infix version of 'comap'
	(<$>) :: (a -> b) -> t a -> t b

	-- | Prefix version of '<$>'
	comap :: (a -> b) -> t a -> t b
	comap f x = f <$> x
	-- | Replace all locations in the input with the same value
	(<$) :: a -> t b -> t a
	(<$) = comap . (!)
	-- | Flipped version of '<$'
	($>) :: t a -> b -> t b
	($>) = (?) (<$)
	-- | Discards the result of evaluation
	void :: t a -> t ()
	void x = () <$ x

instance Covariant ((->) a) where
	(<$>) = (.)
