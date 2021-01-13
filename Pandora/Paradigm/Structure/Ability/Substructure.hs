{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Substructure where

import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (comap)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Paradigm.Inventory.Optics (type (:-.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Substructure f t where
	type Substructural (f :: * -> k) (t :: * -> *) a
	substructure :: Tagged f (t a) :-. Substructural f t a

sub :: forall f t a . Substructure f t => t a :-. Substructural f t a
sub = comap extract . substructure . Tag @f

data Command a = Delete a

data Segment a = Tail a
