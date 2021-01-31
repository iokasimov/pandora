{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Splay where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Paradigm.Primary.Functor (left, right, branches)
import Pandora.Paradigm.Primary.Functor.Function ((%))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (||=))
import Pandora.Paradigm.Schemes (TU (TU))
import Pandora.Paradigm.Structure.Ability.Rotatable (Rotatable (Rotational, rotation), rotate)
import Pandora.Paradigm.Structure.Ability.Substructure (substitute)
import Pandora.Paradigm.Structure.Binary (Binary)
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)

data Splay a = Zig a | Zag a

instance (Avoidable (Rotational r (Nonempty Binary)), Rotatable r (Nonempty Binary)) => Rotatable r Binary where
	type Rotational r Binary = Rotational r (Construction Wye)
	rotation = resolve @(Nonempty Binary _) (rotate @r) empty . run . extract . run

instance Rotatable (Left Zig) (Construction Wye) where
	type Rotational (Left Zig) (Construction Wye) = Binary
	rotation (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> left st where

		subtree = branches (deconstruct <$> left st >>= left) . Just . Construct parent
			$ branches (deconstruct <$> left st >>= right) (right st)

instance Rotatable (Right Zig) (Construction Wye) where
	type Rotational (Right Zig) (Construction Wye) = Binary
	rotation (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> right st where

		subtree = branches (left st) . Just . Construct parent
			$ branches (deconstruct <$> right st >>= left) (deconstruct <$> right st >>= right)

instance Rotatable (Left (Zig Zig)) (Construction Wye) where
	type Rotational (Left (Zig Zig)) (Construction Wye) = Binary
	rotation (extract . run -> tree) = TU $ run (rotate @(Left Zig) tree) >>= run . rotate @(Left Zig)

instance Rotatable (Right (Zig Zig)) (Construction Wye) where
	type Rotational (Right (Zig Zig)) (Construction Wye) = Binary
	rotation (extract . run -> tree) = TU $ run (rotate @(Right Zig) tree) >>= run . rotate @(Right Zig)

instance Rotatable (Left (Zig Zag)) (Construction Wye) where
	type Rotational (Left (Zig Zag)) (Construction Wye) = Binary
	rotation = rotate @(Left Zig) . substitute @Left ((>>= run . rotate @(Right Zig)) ||=) . extract . run

instance Rotatable (Right (Zig Zag)) (Construction Wye) where
	type Rotational (Right (Zig Zag)) (Construction Wye) = Binary
	rotation = rotate @(Right Zig) . substitute @Right ((>>= run . rotate @(Left Zig)) ||=) . extract . run
