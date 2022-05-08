{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.Stream where

import Pandora.Core.Impliable (imply)
import Pandora.Core.Interpreted (run)
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), (-->))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>))
import Pandora.Paradigm.Algebraic.Functor (extract, empty, (--*))
import Pandora.Paradigm.Primary.Auxiliary (Horizontal (Left, Right))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, constitute)
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate), premorph, rotate)
import Pandora.Paradigm.Structure.Interface.Zipper (Zippable (Breadcrumbs, fasten))
import Pandora.Paradigm.Structure.Interface.Stack (Stack (Topping, push, pop, top))
import Pandora.Paradigm.Structure.Modification.Tape (Tape)
import Pandora.Paradigm.Inventory.Some.State (change, current)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Schemes.TT (TT (TT))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U))
import Pandora.Paradigm.Algebraic (point)

type Stream = Construction Exactly

instance Zippable (Construction Exactly) where
	type Breadcrumbs (Construction Exactly) = Reverse Stream <:*:> Stream
	-- TODO: It should not return Maybe since Stream is not empty, try to use Simplification (Fastenable structure) here
	fasten (Construct x xs) = Just <----- Exactly x <:*:> Reverse <-- extract xs <:*:> extract xs

-- TODO: Try to generalize to Extendable (Tape structure)
instance {-# OVERLAPS #-} Extendable (->) (Tape Stream) where
	f <<= z = let move rtt = extract . deconstruct <---- constitute <-- point . rtt <-- z in
		f <-|- imply @(Tape Stream _) <-- z <-- move (rotate @Left) <-- move (rotate @Right)

instance Stack (Construction Exactly) where
	type Topping (Construction Exactly) = Exactly
	top = P_Q_T <-- \xs -> Store <--- Exactly (extract xs) :*: \(Exactly new) -> Construct new <--- deconstruct xs
	pop = (\(Construct x xs) -> constant <-- Exactly x <-|-- change @(Stream _) . constant <-/- xs) =<< current
	push x = (change <-- Construct x . Exactly) --* point x

instance Morphable (Rotate Left) (Tape Stream) where
	type Morphing (Rotate Left) (Tape Stream) = Tape Stream
	morphing (run . premorph -> Exactly x :*: T_U (Reverse ls :*: rs)) =
		imply @(Tape Stream _) <--- extract ls <--- extract (deconstruct ls) <--- Construct x --> point rs

instance Morphable (Rotate Right) (Tape Stream) where
	type Morphing (Rotate Right) (Tape Stream) = Tape Stream
	morphing (run . premorph -> Exactly x :*: T_U (Reverse ls :*: rs)) =
		imply @(Tape Stream _) <--- extract rs <--- Construct x (point ls) <--- extract (deconstruct rs)

repeat :: a -> Stream a
repeat x = Construct x . Exactly <-- repeat x
