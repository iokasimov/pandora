module Pandora.Pattern.Functor.Contravariant (Contravariant (..)) where

import Pandora.Core.Functor (type (:.:), type (><))
import Pandora.Core.Morphism ((.), (!), (?))

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
	($<) = (?) (>$)
	-- | Fill the input of evaluation
	full :: t () -> t a
	full x = () >$ x
	-- | Flipped infix version of 'contramap'
	(>&<) :: t b -> (a -> b) -> t a
	x >&< f = f >$< x

	-- | Infix versions of `contramap` with various nesting levels
	(>$$<) :: Contravariant u => (a -> b) -> t :.: u >< a -> t :.: u >< b
	(>$$<) = (>$<) . (>$<)
	(>$$$<) :: (Contravariant u, Contravariant v)
		=> (a -> b) -> t :.: u :.: v >< b -> t :.: u :.: v >< a
	(>$$$<) = (>$<) . (>$<) . (>$<)
	(>$$$$<) :: (Contravariant u, Contravariant v, Contravariant w)
		=> (a -> b) -> t :.: u :.: v :.: w >< a -> t :.: u :.: v :.: w >< b
	(>$$$$<) = (>$<) . (>$<) . (>$<) . (>$<)

	-- | Infix flipped versions of `contramap` with various nesting levels
	(>&&<) :: Contravariant u => t :.: u >< a -> (a -> b) -> t :.: u >< b
	x >&&< f = f >$$< x
	(>&&&<) :: (Contravariant u, Contravariant v)
		=> t :.: u :.: v >< b -> (a -> b) -> t :.: u :.: v >< a
	x >&&&< f = f >$$$< x
	(>&&&&<) :: (Contravariant u, Contravariant v, Contravariant w)
		=> t :.: u :.: v :.: w >< a -> (a -> b) -> t :.: u :.: v :.: w >< b
	x >&&&&< f = f >$$$$< x
