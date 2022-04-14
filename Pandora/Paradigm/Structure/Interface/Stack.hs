{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Structure.Interface.Stack where

import Pandora.Core.Functor (type (<), type (<<))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Inventory.Some.State (State)
import Pandora.Paradigm.Inventory.Some.Optics (Lens)

{- |
> When providing a new instance, you should ensure it satisfies:
> * Idempotency: item @Push x . morph @Pop ≡ identity
-}

class Stack structure where
	type Topping structure :: * -> *
	top :: Lens < Topping structure < structure e < e
	pop :: State < structure e < Topping structure e
	push :: e -> State < structure e < e
