{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Ability.Substructure where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Divariant ((>->))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Controlflow.Effect.Interpreted ((||=))
import Pandora.Paradigm.Inventory.Optics (type (#=@))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Schemes.TU (type (<:.>))

data Segment a = Root a | Tail a

-- There is no information about Available
type Substructured segment source target = (Substructure segment source, Substance segment source ~ target)

class Substructure segment (structure :: * -> *) where
	type Available segment structure :: * -> *
	type Substance segment structure :: * -> *
	substructure :: (Tagged segment <:.> structure) #=@ Substance segment structure := Available segment structure

	sub :: Covariant structure => structure #=@ Substance segment structure := Available segment structure
	sub = lift >-> ((lower <$>) ||=) ||= substructure @segment @structure
