{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Ability.Modifiable where

import Pandora.Paradigm.Inventory.Ability.Gettable (Gettable)
import Pandora.Paradigm.Inventory.Ability.Settable (Settable)

class (Gettable i, Settable i) => Modifiable i where
	type Modification i e r :: *
	modify :: Modification i e r
