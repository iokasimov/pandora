module Pandora.Pattern.Functor.Covariant (Covariant (..)) where

import Pandora.Core.Functor (type (:.:))
import Pandora.Core.Morphism (fix, (.), ($), (!), (?))

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
	-- | Computing a value from a structure of values
	loeb :: t (t a -> a) -> t a
	loeb tt = fix $ \f -> ($ f) <$> tt
	-- | Infix versions of `comap` with various nesting levels
	(<$$>) :: Covariant u => (a -> b) -> (t :.: u) a -> (t :.: u) b
	(<$$>) = (<$>) . (<$>)
	(<$$$>) :: (Covariant u, Covariant v) => (a -> b) -> (t :.: u :.: v) a -> (t :.: u :.: v) b
	(<$$$>) = (<$>) . (<$>) . (<$>)
	(<$$$$>) :: (Covariant u, Covariant v, Covariant w) => (a -> b) -> (t :.: u :.: v :.: w) a -> (t :.: u :.: v :.: w) b
	(<$$$$>) = (<$>) . (<$>) . (<$>) . (<$>)
	-- | Flipped infix version of 'comap'
	(<&>) :: t a -> (a -> b) -> t b
	x <&> f = f <$> x
	(<&&>) :: Covariant u => (t :.: u) a -> (a -> b) -> (t :.: u) b
	x <&&> f = f <$$> x
	(<&&&>) :: (Covariant u, Covariant v) => (t :.: u :.: v) a -> (a -> b) -> (t :.: u :.: v) b
	x <&&&> f = f <$$$> x
	(<&&&&>) :: (Covariant u, Covariant v, Covariant w) => (t :.: u :.: v :.: w) a -> (a -> b) -> (t :.: u :.: v :.: w) b
	x <&&&&> f = f <$$$$> x

instance Covariant ((->) a) where
	(<$>) = (.)
