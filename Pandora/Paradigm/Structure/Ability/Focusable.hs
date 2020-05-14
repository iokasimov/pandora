module Pandora.Paradigm.Structure.Ability.Focusable (Focusable (..)) where

import Pandora.Core.Functor (type (|->))
import Pandora.Paradigm.Inventory.Optics (type (:-.))

class Focusable t where
	type Focus (t :: * -> *) a
	root :: t a :-. Focus t a
	singleton :: a |-> t
