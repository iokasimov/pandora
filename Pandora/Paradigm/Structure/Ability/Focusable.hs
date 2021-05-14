{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Focusable where

import Pandora.Pattern.Functor.Covariant ((<$>))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Divariant ((>->))
import Pandora.Paradigm.Controlflow.Effect.Interpreted ((||=))
import Pandora.Paradigm.Inventory.Optics (type (:-.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))

class Focusable f t where
	type Focusing (f :: * -> k) (t :: * -> *) a
	focusing :: Tagged f (t a) :-. Focusing f t a

focus :: forall f t a . Focusable f t => t a :-. Focusing f t a
focus = Tag @f >-> (extract <$>) ||= focusing
