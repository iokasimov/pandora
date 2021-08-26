module Pandora.Pattern.Functor.Divariant where

import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip)

infixl 4 >->

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity: identity >-> identity ≡ identity
> * Interpreted: f . g >-> h . i ≡ g >-> h . f >-> i
-}

class (forall i . Contravariant left target (Flip v i), forall i . Covariant right target (v i)) 
	=> Divariant left right target v where
	(>->) :: left a b -> right c d -> target (v b c) (v a d)
