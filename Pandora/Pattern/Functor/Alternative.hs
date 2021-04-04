module Pandora.Pattern.Functor.Alternative where

import Pandora.Pattern.Functor.Covariant (Covariant)

infixl 3 <+>

{- |
> When providing a new instance, you should ensure it satisfies:
> * Associativity of <+>: (x <+> y) <+> z ≡ x <+> (y <+> z)
> * Left-distributes <$> over <+>: f <$> (x <+> y) ≡ (f <$> x) <+> (f <$> y)
-}

class Covariant t => Alternative t where
	{-# MINIMAL (<+>) #-}
	-- | Infix version of 'alter'

	(<+>) :: t a -> t a -> t a
	-- | Prefix version of '<+>'
	alter :: t a -> t a -> t a
	alter f g = f <+> g
