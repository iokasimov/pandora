{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Substructure where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (comap)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Optics (type (:~.), view, over, set)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

class Substructure f t where
	type Substructural (f :: k) (t :: * -> *) :: * -> *
	substructure :: Tagged f <:.> t :~. Substructural f t

	sub :: t :~. Substructural f t
	sub = comap (extract . run) . substructure . TU . Tag @f

	subview :: t ~> Substructural f t
	subview = view (sub @f)

	substitute :: (Substructural f t a -> Substructural f t a) -> t a -> t a
	substitute = over (sub @f)

	subplace :: (Substructural f t) a -> t a -> t a
	subplace = set (sub @f)

data Command a = Delete a

data Segment a = All a | First a | Tail a
