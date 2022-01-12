module Pandora.Paradigm.Structure.Interface.Stack where

import Pandora.Core.Functor (type (#))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Primary.Functor.Identity (Simplification)
import Pandora.Paradigm.Inventory.Some.Equipment (Equipment)
import Pandora.Paradigm.Inventory.Some.State (State)
import Pandora.Paradigm.Inventory.Some.Optics (Lens)

{- |
> When providing a new instance, you should ensure it satisfies:
> * Idempotency: item @Push x . morph @Pop ≡ identity
-}

class Stack t where
	type Topping t :: * -> *
	type Popping t :: * -> *
	push :: e -> State (t e) e
	top :: Lens # Topping t # t e # e
	pop :: t e -> Equipment # Maybe ((Popping t) e) # Simplification (Topping t) e
