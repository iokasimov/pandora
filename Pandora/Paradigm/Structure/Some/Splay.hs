{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Structure.Some.Splay where

import Pandora.Core.Functor (type (~>), type (:.), type (>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), (<------), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Bindable (Bindable ((==<<), (===<<)))
import Pandora.Pattern.Transformer.Hoistable ((/|\))
import Pandora.Paradigm.Algebraic ((<-*-), extract)
import Pandora.Paradigm.Algebraic.Product (type (<:*:>))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Tagged (type (:#))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Primary (twosome)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Some.Optics (view, mutate)
import Pandora.Paradigm.Schemes (TT (TT), type (<::>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morphed, Morph (Rotate), premorph, rotate)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Substructure (Segment (Branch), sub)
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Some.Binary (Binary)

data Splay a = Zig a | Zag a

instance Morphable (Rotate > Left Zig) Binary where
	type Morphing (Rotate > Left Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Left Zig) ==<< run binary

instance Morphable (Rotate > Right Zig) Binary where
	type Morphing (Rotate > Right Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Right Zig) ==<< run binary

instance Morphable (Rotate > Left > Zig Zig) Binary where
	type Morphing (Rotate > Left > Zig Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Left > Zig Zig) ==<< run binary

instance Morphable (Rotate > Right > Zig Zig) Binary where
	type Morphing (Rotate > Right > Zig Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Right > Zig Zig) ==<< run binary

instance Morphable (Rotate > Left > Zig Zag) Binary where
	type Morphing (Rotate > Left > Zig Zag) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Left > Zig Zag) ==<< run binary

instance Morphable (Rotate > Right > Zig Zag) Binary where
	type Morphing (Rotate > Right > Zig Zag) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Right > Zig Zag) ==<< run binary

-------------------------------------- Non-empty Splay tree ----------------------------------------

-- TODO: refactor so that there is only one expression
instance Morphable (Rotate > Left Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Left Zig) (Construction (Maybe <:*:> Maybe)) = Binary
	morphing :: forall a . (:#) (Rotate > Left Zig) <::> Construction (Maybe <:*:> Maybe) > a -> Binary a
	morphing (premorph -> Construct x xs) = TT <--- Construct <-|- parent <-*- Just nodes where

		nodes :: (Maybe <:*:> Maybe) :. Nonempty Binary > a
		nodes = twosome
			<------ view <-- sub @Left <-- xs
			<------ Just . Construct x
				<----- twosome
					-- TODO: to refactor this we can define Substructure t => Substructure (Construction t) instance
					<---- view (sub @Left) ===<< deconstruct <-|- view (sub @Right) xs
					<---- view (sub @Right) ===<< deconstruct <-|- view (sub @Right) xs

		parent :: Maybe a
		parent = extract <-|-- view <-- sub @Right <-- xs

instance Morphable (Rotate > Right Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Right Zig) (Construction (Maybe <:*:> Maybe)) = Binary
	morphing :: forall a . (:#) (Rotate > Right Zig) <::> Construction (Maybe <:*:> Maybe) > a -> Binary a
	morphing (premorph -> Construct x xs) = TT <--- Construct <-|- parent <-*- Just nodes where

		nodes :: (Maybe <:*:> Maybe) :. Nonempty Binary > a
		nodes = twosome
			<------ view (sub @Left) ===<< deconstruct <-|- view (sub @Left) xs
			<------ Just . Construct x
				<----- twosome
					<---- view (sub @Right) ===<< deconstruct <-|- view (sub @Left) xs
					<---- view (sub @Right) xs

		parent :: Maybe a
		parent = extract <-|-- view <-- sub @Left <-- xs

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Left > Zig Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Left > Zig Zig) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> tree) = TT <---- run . rotate @(Left Zig) ===<< run <-- rotate @(Left Zig) tree

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Right > Zig Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Right > Zig Zig) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> tree) = TT <---- run . rotate @(Right Zig) ===<< run <-- rotate @(Right Zig) tree

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Left > Zig Zag) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Left > Zig Zag) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> struct) = rotate @(Left Zig) <--- mutate <-- (try_to_rotate @(Right Zig) /|\) <-- sub @(Left Branch) <-- struct

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Right > Zig Zag) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Right > Zig Zag) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> struct) = rotate @(Right Zig) <--- mutate <-- (try_to_rotate @(Left Zig) /|\) <-- sub @(Right Branch) <-- struct

-- TODO: Include error instead of returning empty tree
try_to_rotate :: forall direction . Morphed (Rotate direction) (Nonempty Binary) Binary => Nonempty Binary ~> Nonempty Binary
try_to_rotate tree = resolve @(Nonempty Binary _) identity tree <--- run <-- rotate @direction tree
