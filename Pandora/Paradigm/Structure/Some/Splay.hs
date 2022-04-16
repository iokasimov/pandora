{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Structure.Some.Splay where

import Pandora.Core.Functor (type (~>), type (>), type (>>>), type (>>>>>>))
import Pandora.Core.Interpreted (run)
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|---)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<), (==<<), (===<<), (====<<)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Hoistable ((/|\))
import Pandora.Paradigm.Algebraic.Functor ((<-*-), extract, point, void)
import Pandora.Paradigm.Algebraic.Product (type (<:*:>), (<:*:>))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left_, Right_))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (adapt)
import Pandora.Paradigm.Controlflow.Effect.Transformer ((:>), wrap)
import Pandora.Paradigm.Inventory.Some.Optics (view, mutate)
import Pandora.Paradigm.Inventory.Some.State (State, change, current)
import Pandora.Paradigm.Inventory (zoom, overlook)
import Pandora.Paradigm.Schemes (TT (TT), type (<::>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morphed, Morph (Rotate), premorph, rotate)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Substructure (Segment (Root, Branch), sub)
import Pandora.Paradigm.Structure.Ability.Slidable (Slidable (Sliding, slide))
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Some.Binary (Binary)

data Splay a = Zig a | Zag a

instance Morphable (Rotate > Left_ Zig) Binary where
	type Morphing (Rotate > Left_ Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Left_ Zig) =<< run binary

instance Morphable (Rotate > Right_ Zig) Binary where
	type Morphing (Rotate > Right_ Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Right_ Zig) =<< run binary

instance Morphable (Rotate > Left_ > Zig Zig) Binary where
	type Morphing (Rotate > Left_ > Zig Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Left_ > Zig Zig) =<< run binary

instance Morphable (Rotate > Right_ > Zig Zig) Binary where
	type Morphing (Rotate > Right_ > Zig Zig) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Right_ > Zig Zig) =<< run binary

instance Morphable (Rotate > Left_ > Zig Zag) Binary where
	type Morphing (Rotate > Left_ > Zig Zag) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Left_ > Zig Zag) =<< run binary

instance Morphable (Rotate > Right_ > Zig Zag) Binary where
	type Morphing (Rotate > Right_ > Zig Zag) Binary = Binary
	morphing (premorph -> binary) = TT <--- run . rotate @(Right_ > Zig Zag) =<< run binary

-------------------------------------- Non-empty Splay tree ----------------------------------------

instance Morphable (Rotate > Left_ Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Left_ Zig) (Construction (Maybe <:*:> Maybe)) = Binary
	morphing (premorph -> tree) = TT <---- Construct
		<-|- (extract <-|--- run <--- view <-- sub @(Right_ Branch) <-- tree)
		<-*- Just (
			(<:*:>)
				(run <--- view <-- sub @(Left_ Branch) <-- tree)
				(Just . Construct (extract <--- view <-- sub @Root <-- tree) <-- (<:*:>)
					(run . view (sub @(Left_ Branch)) ===<< run <--- view <-- sub @(Right_ Branch) <-- tree)
					(run . view (sub @(Right_ Branch)) ===<< run <--- view <-- sub @(Right_ Branch) <-- tree)
				)
			)

instance Morphable (Rotate > Right_ Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Right_ Zig) (Construction (Maybe <:*:> Maybe)) = Binary
	morphing (premorph -> tree) = TT <---- Construct
		<-|- (extract <-|--- run <--- view <-- sub @(Left_ Branch) <-- tree)
		<-*- Just (
			(<:*:>)
				(run . view (sub @(Left_ Branch)) ===<< run <--- view <-- sub @(Left_ Branch) <-- tree)
				(Just . Construct (extract <--- view <-- sub @Root <-- tree) <-- (<:*:>)
					(run . view (sub @(Left_ Branch)) ===<< run <--- view <-- sub @(Left_ Branch) <-- tree)
					(run <--- view <-- sub @(Right_ Branch) <-- tree)
				)
			)

-- TODO: Slidable (Left_ Zig) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Left_ > Zig Zig) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Right_ > Zig Zig) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Left_ > Zig Zag) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Right_ > Zig Zag) (Construction (Maybe <:*:> Maybe))
-- TODO: Slidable (Left_ Zig) Binary
-- TODO: Slidable (Right_ Zig) Binary
-- TODO: Slidable (Left_ > Zig Zig) Binary
-- TODO: Slidable (Right_ > Zig Zig) Binary
-- TODO: Slidable (Left_ > Zig Zag) Binary
-- TODO: Slidable (Right_ > Zig Zag) Binary

instance Slidable (Right_ Zig) (Construction (Maybe <:*:> Maybe)) where
	type Sliding (Right_ Zig) (Construction (Maybe <:*:> Maybe)) = Maybe
	slide :: forall element . State > Nonempty Binary element :> Maybe >>> ()
	slide = void . point
		===<< adapt . zoom @(Nonempty Binary element) (sub @(Right_ Branch)) .
			zoom (sub @(Left_ Branch)) . change . constant
		===<< adapt . zoom @(Nonempty Binary element) (sub @(Right_ Branch))
			. change @(Binary element) . constant . lift
		===<< change . constant ===<< adapt . run
		===<< adapt <--- zoom @(Nonempty Binary element)
			<-- sub @(Left_ Branch)
			<-- current @(Binary element)

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Left_ > Zig Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Left_ > Zig Zig) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> tree) = TT <---- run . rotate @(Left_ Zig) ==<< run <-- rotate @(Left_ Zig) tree

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Right_ > Zig Zig) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Right_ > Zig Zig) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> tree) = TT <---- run . rotate @(Right_ Zig) ==<< run <-- rotate @(Right_ Zig) tree

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Left_ > Zig Zag) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Left_ > Zig Zag) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> struct) = rotate @(Left_ Zig) <--- mutate <-- (try_to_rotate @(Right_ Zig) /|\) <-- sub @(Left_ Branch) <-- struct

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate > Right_ > Zig Zag) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Rotate > Right_ > Zig Zag) (Construction (Maybe <:*:> Maybe)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	morphing (premorph -> struct) = rotate @(Right_ Zig) <--- mutate <-- (try_to_rotate @(Left_ Zig) /|\) <-- sub @(Right_ Branch) <-- struct

-- TODO: Include error instead of returning empty tree
try_to_rotate :: forall direction . Morphed (Rotate direction) (Nonempty Binary) Binary => Nonempty Binary ~> Nonempty Binary
try_to_rotate tree = resolve @(Nonempty Binary _) identity tree <--- run <-- rotate @direction tree
