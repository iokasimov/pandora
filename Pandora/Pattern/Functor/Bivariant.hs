module Pandora.Pattern.Functor.Bivariant where

import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip)

infixl 4 <->

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity: identity <-> identity ≡ identity
> * Parametricity: (f . g) <-> (h . i) ≡ f <-> h . g <-> i
-}

class (forall i . Covariant (v i) left target, forall i . Covariant (Flip v i) right target)
	=> Bivariant v left right target where
	(<->) :: left a b -> right c d -> target (v a c) (v b d) 
