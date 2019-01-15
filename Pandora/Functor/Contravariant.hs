module Pandora.Functor.Contravariant (Contravariant (..)) where

import Pandora.Morphism ((.), (!), flip)

infixl 4 >$<, $<, >$

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Identity morphism: contramap identity ≡ identity
> * Composition of morphisms: contramap f . contramap g ≡ contramap (g . f)
-}

class Contravariant (t :: * -> *) where
	{-# MINIMAL (>$<) #-}
	-- | Infix version of 'contramap'
	(>$<) :: (a -> b) -> t b -> t a

	-- | Prefix version of '>$<'
	contramap :: (a -> b) -> t b -> t a
	contramap f x = f >$< x
	-- | Replace all locations in the output with the same value
	(>$) :: b -> t b -> t a
	(>$) = contramap . (!)
	-- | Flipped version of '>$'
	($<) :: t b -> b -> t a
	($<) = flip (>$)
	-- | Fill the input of evaluation
	full :: t () -> t a
	full x = () >$ x
