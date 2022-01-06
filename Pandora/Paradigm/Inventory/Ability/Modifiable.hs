{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Inventory.Some.Ability.Modifiable where

import Pandora.Paradigm.Inventory.Some.Ability.Viewable (Viewable)
import Pandora.Paradigm.Inventory.Some.Ability.Replaceable (Replaceable)

class (Viewable i, Replaceable i) => Modifiable i where
	type Modification i l r :: *
	modify_ :: Modification i l r
