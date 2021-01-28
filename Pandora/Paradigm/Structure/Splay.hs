{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Splay where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Paradigm.Primary.Functor (left, right, branches)
import Pandora.Paradigm.Primary.Functor.Function ((%))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (||=))
import Pandora.Paradigm.Inventory.Optics ((%~))
import Pandora.Paradigm.Schemes (TU (TU), type (<:.>))
import Pandora.Paradigm.Structure.Ability.Rotatable (Rotatable (Rotational, rotation), rotate)
import Pandora.Paradigm.Structure.Ability.Substructure (sub)
import Pandora.Paradigm.Structure.Binary ()

data Splay a = Zig a | Zag a

instance Rotatable (Left Zig) (Construction Wye) where
	type Rotational (Left Zig) (Construction Wye) = Maybe <:.> Construction Wye
	rotation (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> left st where

		subtree = branches (deconstruct <$> left st >>= left) . Just . Construct parent
			$ branches (deconstruct <$> left st >>= right) (right st)

instance Rotatable (Right Zig) (Construction Wye) where
	type Rotational (Right Zig) (Construction Wye) = Maybe <:.> Construction Wye
	rotation (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> right st where

		subtree = branches (left st) . Just . Construct parent
			$ branches (deconstruct <$> right st >>= left) (deconstruct <$> right st >>= right)

instance Rotatable (Left (Zig Zig)) (Construction Wye) where
	type Rotational (Left (Zig Zig)) (Construction Wye) = Maybe <:.> Construction Wye
	rotation (extract . run -> tree) = TU $ run (rotate @(Left Zig) tree) >>= run . rotate @(Left Zig)

instance Rotatable (Right (Zig Zig)) (Construction Wye) where
	type Rotational (Right (Zig Zig)) (Construction Wye) = Maybe <:.> Construction Wye
	rotation (extract . run -> tree) = TU $ run (rotate @(Right Zig) tree) >>= run . rotate @(Right Zig)

instance Rotatable (Left (Zig Zag)) (Construction Wye) where
	type Rotational (Left (Zig Zag)) (Construction Wye) = Maybe <:.> Construction Wye
	rotation (extract . run -> tree) = rotate @(Left Zig)
		$ sub @Left %~ ((>>= run . rotate @(Right Zig)) ||=) $ tree

instance Rotatable (Right (Zig Zag)) (Construction Wye) where
	type Rotational (Right (Zig Zag)) (Construction Wye) = Maybe <:.> Construction Wye
	rotation (extract . run -> tree) = rotate @(Right Zig)
		$ sub @Right %~ ((>>= run . rotate @(Left Zig)) ||=) $ tree
