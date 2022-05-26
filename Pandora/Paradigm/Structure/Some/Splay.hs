{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Structure.Some.Splay where

import Pandora.Core.Functor (type (~>), type (>), type (>>>))
import Pandora.Core.Interpreted (run, (<~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Category ((<--), (<---), (<----), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|---)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<), (==<<), (===<<)))
import Pandora.Pattern.Transformation.Liftable (lift)
import Pandora.Pattern.Transformation.Hoistable ((/|\))
import Pandora.Paradigm.Algebraic.Functor ((<-*-), extract, point, void)
import Pandora.Paradigm.Algebraic.Product (type (<:*:>), (<:*:>), attached)
import Pandora.Paradigm.Primary.Auxiliary (Horizontal (Left, Right))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (adapt)
import Pandora.Paradigm.Controlflow.Effect.Transformer ((:>))
import Pandora.Paradigm.Inventory.Some.Optics (view, mutate)
import Pandora.Paradigm.Inventory.Some.State (State, change, current)
import Pandora.Paradigm.Inventory (zoom)
import Pandora.Paradigm.Schemes (TT (TT), type (<::>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morphed, Morph (Rotate), premorph, rotate)
import Pandora.Paradigm.Structure.Modification.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Substructure (Segment (Root, Branch), sub)
import Pandora.Paradigm.Structure.Ability.Slidable (Slidable (Sliding, slide))
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Some.Binary (Binary)

data Splay a = Zig a | Zag a

instance Morphable (Rotate > Left Zig) Binary where
	type Morphing (Rotate > Left Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Left Zig) =<< run binary

instance Morphable (Rotate > Right Zig) Binary where
	type Morphing (Rotate > Right Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Right Zig) =<< run binary

instance Morphable (Rotate > Left > Zig Zig) Binary where
	type Morphing (Rotate > Left > Zig Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Left > Zig Zig) =<< run binary

instance Morphable (Rotate > Right > Zig Zig) Binary where
	type Morphing (Rotate > Right > Zig Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Right > Zig Zig) =<< run binary

instance Morphable (Rotate > Left > Zig Zag) Binary where
	type Morphing (Rotate > Left > Zig Zag) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Left > Zig Zag) =<< run binary

instance Morphable (Rotate > Right > Zig Zag) Binary where
	type Morphing (Rotate > Right > Zig Zag) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Right > Zig Zag) =<< run binary

-------------------------------------- Non-empty Splay tree ----------------------------------------

instance Morphable (Rotate > Left Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Left Zig) (Construction (Maybe <:*:> Maybe)) = Binary
	morphing (premorph -> tree) = TT <---- Construct
		<-|- (extract <-|--- run <--- view <-- sub @(Right Branch) <-- tree)
		<-*- Just (
			(<:*:>)
				(run <--- view <-- sub @(Left Branch) <-- tree)
				(Just . Construct (extract <--- view <-- sub @Root <-- tree) <-- (<:*:>)
					(run . view (sub @(Left Branch)) ===<< run <--- view <-- sub @(Right Branch) <-- tree)
					(run . view (sub @(Right Branch)) ===<< run <--- view <-- sub @(Right Branch) <-- tree)
				)
			)

instance Morphable (Rotate > Right Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Right Zig) (Construction (Maybe <:*:> Maybe)) = Binary
	morphing (premorph -> tree) = TT <---- Construct
		<-|- (extract <-|--- run <--- view <-- sub @(Left Branch) <-- tree)
		<-*- Just (
			(<:*:>)
				(run . view (sub @(Left Branch)) ===<< run <--- view <-- sub @(Left Branch) <-- tree)
				(Just . Construct (extract <--- view <-- sub @Root <-- tree) <-- (<:*:>)
					(run . view (sub @(Left Branch)) ===<< run <--- view <-- sub @(Left Branch) <-- tree)
					(run <--- view <-- sub @(Right Branch) <-- tree)
				)
			)

-- TODO: Slidable (Left > Zig) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Left > Zig Zig) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Right > Zig Zig) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Left > Zig Zag) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Right > Zig Zag) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Left > Zig) Binary
-- TODO: Slidable (Left > Zig Zig) Binary
-- TODO: Slidable (Right > Zig Zig) Binary
-- TODO: Slidable (Left > Zig Zag) Binary
-- TODO: Slidable (Right > Zig Zag) Binary

instance Slidable (Right Zig) (Construction (Maybe <:*:> Maybe)) where
	type Sliding (Right Zig) (Construction (Maybe <:*:> Maybe)) = Maybe
	slide :: forall element . State > Nonempty Binary element :> Maybe >>> ()
	slide = void . point
		===<< adapt . zoom @(Nonempty Binary element) (sub @(Right Branch)) .
			zoom (sub @(Left Branch)) . change . constant
		===<< adapt . zoom @(Nonempty Binary element) (sub @(Right Branch))
			. change @(Binary element) . constant . lift
		===<< change . constant ===<< adapt . run
		===<< adapt <--- zoom @(Nonempty Binary element)
			<-- sub @(Left Branch)
			<-- current @(Binary element)

instance Slidable (Right > Zig) Binary where
	type Sliding (Right Zig) Binary = Maybe
	slide :: forall element . State > Binary element :> Maybe >>> ()
	slide = void . change @(Binary element) . constant . lift . attached
		=<< adapt . (slide @(Right > Zig) @(Nonempty Binary) <~)
		=<< adapt . run =<< current @(Binary element)

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Left > Zig Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Left > Zig Zig) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> tree) = TT <---- run . rotate @(Left Zig) ==<< run <-- rotate @(Left Zig) tree

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Right > Zig Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Right > Zig Zig) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> tree) = TT <---- run . rotate @(Right Zig) ==<< run <-- rotate @(Right Zig) tree

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
