{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Structure.Some.Splay where

import Pandora.Core.Functor (type (~>), type (:.), type (:=))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((#), (<-.-), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Paradigm.Primary ()
import Pandora.Paradigm.Primary.Algebraic ((<-*-), extract)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Tagged (type (:#))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Primary (twosome)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (!))
import Pandora.Paradigm.Inventory.Ability.Modifiable (modify)
import Pandora.Paradigm.Inventory.Some.Optics (Lens, Obscure)
import Pandora.Paradigm.Schemes (TT (TT), type (<::>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morphed, Morph (Rotate, Into), premorph, rotate, into)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Substructure (sub)
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Some.Binary (Binary)

data Splay a = Zig a | Zag a

instance Morphable (Rotate (Left Zig)) Binary where
	type Morphing (Rotate (Left Zig)) Binary = Binary
	morphing (premorph -> binary) = TT ! run . rotate @(Left Zig) =<< run binary

instance Morphable (Rotate (Right Zig)) Binary where
	type Morphing (Rotate (Right Zig)) Binary = Binary
	morphing (premorph -> binary) = TT ! run . rotate @(Right Zig) =<< run binary

instance Morphable (Rotate (Left (Zig Zig))) Binary where
	type Morphing (Rotate (Left (Zig Zig))) Binary = Binary
	morphing (premorph -> binary) = TT ! run . rotate @(Left (Zig Zig)) =<< run binary

instance Morphable (Rotate (Right (Zig Zig))) Binary where
	type Morphing (Rotate (Right (Zig Zig))) Binary = Binary
	morphing (premorph -> binary) = TT ! run . rotate @(Right (Zig Zig)) =<< run binary

instance Morphable (Rotate (Left (Zig Zag))) Binary where
	type Morphing (Rotate (Left (Zig Zag))) Binary = Binary
	morphing (premorph -> binary) = TT ! run . rotate @(Left (Zig Zag)) =<< run binary

instance Morphable (Rotate (Right (Zig Zag))) Binary where
	type Morphing (Rotate (Right (Zig Zag))) Binary = Binary
	morphing (premorph -> binary) = TT ! run . rotate @(Right (Zig Zag)) =<< run binary

-------------------------------------- Non-empty Splay tree ----------------------------------------

instance Morphable (Rotate (Left Zig)) (Construction Wye) where
	type Morphing (Rotate (Left Zig)) (Construction Wye) = Binary
	morphing :: forall a . (:#) (Rotate (Left Zig)) <::> Construction Wye := a -> Binary a
	morphing (premorph -> Construct x xs) = TT ! Construct <-|- parent <-*- Just nodes where

		nodes :: Wye :. Nonempty Binary := a
		nodes = into @Wye . twosome (branch @Left xs) . Just . Construct x
			. into @Wye ! twosome (branch @Left =<< deconstruct <-|- branch @Right xs)
				(branch @Right =<< deconstruct <-|- branch @Right xs)

		parent :: Maybe a
		parent = extract <-|- branch @Right xs

instance Morphable (Rotate (Right Zig)) (Construction Wye) where
	type Morphing (Rotate (Right Zig)) (Construction Wye) = Binary
	morphing :: forall a . (:#) (Rotate (Right Zig)) <::> Construction Wye := a -> Binary a
	morphing (premorph -> Construct x xs) = TT ! Construct <-|- parent <-*- Just nodes where

		nodes :: Wye :. Nonempty Binary := a
		nodes = into @Wye . twosome (branch @Left =<< deconstruct <-|- branch @Left xs) . Just . Construct x
			. into @Wye ! twosome (branch @Right =<< deconstruct <-|- branch @Left xs) # branch @Right xs

		parent :: Maybe a
		parent = extract <-|- branch @Left xs

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate (Left (Zig Zig))) (Construction Wye) where
	type Morphing (Rotate (Left (Zig Zig))) (Construction Wye) = Maybe <::> Construction Wye
	morphing (premorph -> tree) = TT ! run . rotate @(Left Zig) =<< run <-.- rotate @(Left Zig) tree

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate (Right (Zig Zig))) (Construction Wye) where
	type Morphing (Rotate (Right (Zig Zig))) (Construction Wye) = Maybe <::> Construction Wye
	morphing (premorph -> tree) = TT ! run . rotate @(Right Zig) =<< run <-.- rotate @(Right Zig) tree

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate (Left (Zig Zag))) (Construction Wye) where
	type Morphing (Rotate (Left (Zig Zag))) (Construction Wye) = Maybe <::> Construction Wye
	morphing (premorph -> struct) = rotate @(Left Zig) ! modify @(Obscure Lens) <-.- try_to_rotate @(Right Zig) <-.- sub @Left <-.- struct

-- TODO: Morphing ... = Conclussion Error <::> Nonempty Binary
instance Morphable (Rotate (Right (Zig Zag))) (Construction Wye) where
	type Morphing (Rotate (Right (Zig Zag))) (Construction Wye) = Maybe <::> Construction Wye
	morphing (premorph -> struct) = rotate @(Right Zig) ! modify @(Obscure Lens) <-.- try_to_rotate @(Left Zig) <-.- sub @Right <-.- struct

branch :: forall b . Morphable (Into (b Maybe)) Wye => Wye ~> Morphing (Into (b Maybe)) Wye
branch = into @(b Maybe)

try_to_rotate :: forall direction . Morphed (Rotate direction) (Nonempty Binary) Binary => Nonempty Binary ~> Nonempty Binary
try_to_rotate tree = resolve @(Nonempty Binary _) identity tree ! run # rotate @direction tree
