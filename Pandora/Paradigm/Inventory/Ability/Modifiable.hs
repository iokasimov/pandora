{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Ability.Modifiable where

import Pandora.Paradigm.Inventory.Ability.Gettable (Gettable)
import Pandora.Paradigm.Inventory.Ability.Replaceable (Replaceable)

class (Gettable i, Replaceable i) => Modifiable i where
	type Modification i e r :: *
	modify :: Modification i e r
