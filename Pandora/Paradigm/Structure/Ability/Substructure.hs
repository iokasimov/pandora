{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Ability.Substructure where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Divariant ((>->))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Controlflow.Effect.Interpreted ((||=))
import Pandora.Paradigm.Inventory.Optics (type (:~.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Schemes.TU (type (<:.>))

class Substructure f t where
	type Substructural (f :: k) (t :: * -> *) :: * -> *
	substructure :: Tagged f <:.> t :~. Substructural f t

	sub :: Covariant t => t :~. Substructural f t
	sub = lift >-> (lower <$>) ||= substructure @f @t

data Segment a = Root a | Tail a

type Substructured i source target = (Substructure i source, Substructural i source ~ target)

-- class Substructure' f t where
-- 	type Substructural' (f :: k) (t :: * -> *) :: * -> *
-- 	substructure' :: Tagged f <:.> t :~. Substructural' f t
