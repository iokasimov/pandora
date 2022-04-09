{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Structure.Ability.Slidable where

import Pandora.Core.Functor (type (>>>), type (<))
import Pandora.Paradigm.Controlflow.Effect.Transformer ((:>))
import Pandora.Paradigm.Inventory.Some.State (State)

class Slidable d (s :: * -> *) where
	type Sliding d s :: * -> *
	slide :: State < s e :> Sliding d s >>> ()
