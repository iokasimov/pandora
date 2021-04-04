module Pandora.Pattern.Functor.Contravariant where

import Pandora.Core.Functor (type (:.), type (:=))

infixl 4 >$<, $<, >$

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: contramap identity ≡ identity
> * Interpreted of morphisms: contramap f . contramap g ≡ contramap (g . f)
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
	x >$ z = (\_ -> x) >$< z
	-- | Flipped version of '>$'
	($<) :: t b -> b -> t a
	x $< v = v >$ x
	-- | Fill the input of evaluation
	full :: t () -> t a
	full x = () >$ x
	-- | Flipped infix version of 'contramap'
	(>&<) :: t b -> (a -> b) -> t a
	x >&< f = f >$< x

	-- | Infix versions of `contramap` with various nesting levels
	(>$$<) :: Contravariant u => (a -> b) -> t :. u := a -> t :. u := b
	f >$$< x = ((f >$<) >$<) x
	(>$$$<) :: (Contravariant u, Contravariant v)
		=> (a -> b) -> t :. u :. v := b -> t :. u :. v := a
	f >$$$< x = (((f >$<) >$<) >$<) x
	(>$$$$<) :: (Contravariant u, Contravariant v, Contravariant w)
		=> (a -> b) -> t :. u :. v :. w := a -> t :. u :. v :. w := b
	f >$$$$< x = ((((f >$<) >$<) >$<) >$<) x

	-- | Infix flipped versions of `contramap` with various nesting levels
	(>&&<) :: Contravariant u => t :. u := a -> (a -> b) -> t :. u := b
	x >&&< f = f >$$< x
	(>&&&<) :: (Contravariant u, Contravariant v)
		=> t :. u :. v := b -> (a -> b) -> t :. u :. v := a
	x >&&&< f = f >$$$< x
	(>&&&&<) :: (Contravariant u, Contravariant v, Contravariant w)
		=> t :. u :. v :. w := a -> (a -> b) -> t :. u :. v :. w := b
	x >&&&&< f = f >$$$$< x
