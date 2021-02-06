{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Ability.Accessible where

import Pandora.Paradigm.Inventory.Optics (type (:-.), (|>))

class Accessible tgt src where
	access :: src :-. tgt

instance (Accessible b a, Accessible c b) => Accessible c a where
	access = access @b @a |> access @c @b
