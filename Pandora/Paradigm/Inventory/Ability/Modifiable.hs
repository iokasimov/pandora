{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Ability.Modifiable where

import Pandora.Paradigm.Inventory.Ability.Viewable (Viewable)
import Pandora.Paradigm.Inventory.Ability.Replaceable (Replaceable)

class (Viewable i, Replaceable i) => Modifiable i where
	type Modification i l r :: *
	modify_ :: Modification i l r
