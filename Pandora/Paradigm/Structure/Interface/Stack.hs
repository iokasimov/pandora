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

class Stack t where
	type Topping t :: * -> *
	type Popping t :: * -> *
	type Pushing t :: * -> *
	top :: Lens < Topping t < t e < e
	-- TODO: In case of nonempty list we know that we return an element
	pop :: State << Popping t < e << Maybe e
	push :: e -> State << Pushing t < e <<  e
