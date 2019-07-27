module Pandora.Pattern.Functor.Divariant (Divariant (..)) where

import Pandora.Pattern.Functor.Covariant (Covariant)

import Pandora.Core.Morphism ((.))

infixl 4 >->
infixr 0 $

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Identity: dimap identity identity ≡ identity
> * Composition: dimap (f . g) (h . i) ≡ dimap g h . dimap f i
-}

class (forall a . Covariant (t a)) => Divariant (t :: * -> * -> *) where
	{-# MINIMAL (>->) #-}
	-- | Infix version of 'comap'
	(>->) :: (a -> b) -> (c -> d) -> t b c -> t a d

	-- | Prefix version of '>->'
	dimap :: (a -> b) -> (c -> d) -> t b c -> t a d
	dimap f g x = (f >-> g) x

	($) :: t a b -> t a b
	($) f = f

instance Divariant ((->)) where
	(>->) ab cd bc = cd . bc . ab
