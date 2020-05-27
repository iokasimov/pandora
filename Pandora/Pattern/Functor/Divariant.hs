module Pandora.Pattern.Functor.Divariant (Divariant (..)) where

import Pandora.Pattern.Category ((.), ($))

infixl 4 >->

{- |
> When providing a new instance, you should ensure it satisfies the two laws:
> * Identity: dimap identity identity ≡ identity
> * Interpreted: dimap (f . g) (h . i) ≡ dimap g h . dimap f i
-}

class Divariant (v :: * -> * -> *) where
	{-# MINIMAL (>->) #-}
	(>->) :: v a b -> v c d -> v b c -> v a d
	-- | Prefix version of '>->'
	dimap :: v a b -> v c d -> v b c -> v a d
	dimap f g x = f >-> g $ x

instance Divariant ((->)) where
	(>->) ab cd bc = cd . bc . ab
