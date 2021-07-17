module Pandora.Pattern.Functor.Divariant where

import Pandora.Pattern.Functor.Covariant (Covariant_)
import Pandora.Pattern.Functor.Contravariant (Contravariant_)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip)

infixl 4 ->->-

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity: identity ->->- identity ≡ identity
> * Interpreted: f . g ->->- h . i ≡ g ->->- h . f ->->- i
-}

--class (forall i . Covariant (v i)) => Divariant (v :: * -> * -> *) where
--	{-# MINIMAL (>->) #-}
--	(>->) :: (a -> b) -> (c -> d) -> v b c -> v a d
--	-- | Prefix version of '>->'
--	dimap :: (a -> b) -> (c -> d) -> v b c -> v a d
--	dimap f g x = (f >-> g) x

class (forall i . Contravariant_ (Flip v i) left target, forall i . Covariant_ (v i) right target) 
	=> Divariant_ v left right target where
	(->->-) :: left a b -> right c d -> target (v b c) (v a d)
