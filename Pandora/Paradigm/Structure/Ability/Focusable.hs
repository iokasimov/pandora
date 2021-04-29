{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Focusable where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant ((<$>))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Optics (type (:-.))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Schemes.PQ_ (PQ_ (PQ_))

class Focusable f t where
	type Focusing (f :: * -> k) (t :: * -> *) a
	focusing :: Tagged f (t a) :-. Focusing f t a

focus :: forall f t a . Focusable f t => t a :-. Focusing f t a
focus = PQ_ $ \x -> extract <$> run focusing (Tag @f x)

data Location a = Root a | Head a
