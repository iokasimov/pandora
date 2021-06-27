module Pandora.Pattern.Functor.Bivariant where

import Pandora.Pattern.Functor.Covariant (Covariant, Covariant_)
import Pandora.Pattern.Functor.Contravariant (Contravariant_)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip)
import Pandora.Paradigm.Controlflow.Effect.Interpreted ((||=))

infixl 4 <->

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity: bimap identity identity ≡ identity
> * Parametricity: bimap  (f . g) (h . i) ≡ bimap f h . bimap g i
-}

class (forall i . Covariant (v i)) => Bivariant (v :: * -> * -> *) where
	{-# MINIMAL (<->) #-}
	(<->) :: (forall i . Covariant (v i)) => (a -> b) -> (c -> d) -> v a c -> v b d

	-- | Prefix version of '<->'
	bimap :: (forall i . Covariant (v i)) => (a -> b) -> (c -> d) -> v a c -> v b d
	bimap f g x = (f <-> g) x

class (forall i . Covariant_ (v i) left target, forall i . Covariant_ (Flip v i) right target)
	=> Bivariant_ v left right target where
	(-<->-) :: left a b -> right c d -> target (v a c) (v b d) 
