module Pandora.Paradigm.Structure.Ability.Focusable where

import Pandora.Core.Functor (type (|->))
import Pandora.Paradigm.Inventory.Optics (type (:-.))

class Focusable t where
	type Focus (t :: * -> *) a
	top :: t a :-. Focus t a
	singleton :: a |-> t
