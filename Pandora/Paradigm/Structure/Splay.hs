{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Splay where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Paradigm.Primary.Functor (left, right, branches)
import Pandora.Paradigm.Primary.Functor.Function ((%))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag), type (:#))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Optics ((%~))
import Pandora.Paradigm.Schemes (TU (TU))
import Pandora.Paradigm.Structure.Ability.Rotatable (Rotatable (Rotational, rotation), rotate)
import Pandora.Paradigm.Structure.Ability.Substructure (sub)
import Pandora.Paradigm.Structure.Binary ()

data Splay a = Zig a | Zag a

instance Rotatable (Left Zig) (Construction Wye) where
	type Rotational (Left Zig) (Construction Wye) a = Maybe (Construction Wye a)
	rotation :: forall a . Left Zig :# Construction Wye a -> Maybe :. Construction Wye := a
	rotation (extract -> Construct parent st) = Construct % subtree <$> found where

		found :: Maybe a
		found = extract <$> left st

		subtree :: Wye :. Construction Wye := a
		subtree = branches (deconstruct <$> left st >>= left)
			. Just . Construct parent $ branches
				(deconstruct <$> left st >>= right) (right st)

instance Rotatable (Right Zig) (Construction Wye) where
	type Rotational (Right Zig) (Construction Wye) a = Maybe (Construction Wye a)
	rotation :: forall a . Right Zig :# Construction Wye a -> Maybe :. Construction Wye := a
	rotation (extract -> Construct parent st) = Construct % subtree <$> found where

		found :: Maybe a
		found = extract <$> right st

		subtree :: Wye :. Construction Wye := a
		subtree = branches (left st) . Just . Construct parent $ branches
			(deconstruct <$> right st >>= left) (deconstruct <$> right st >>= right)

instance Rotatable (Left (Zig Zig)) (Construction Wye) where
	type Rotational (Left (Zig Zig)) (Construction Wye) a = Maybe (Construction Wye a)
	rotation (Tag tree) = rotate @(Left Zig) tree >>= rotate @(Left Zig)

instance Rotatable (Right (Zig Zig)) (Construction Wye) where
	type Rotational (Right (Zig Zig)) (Construction Wye) a = Maybe (Construction Wye a)
	rotation (Tag tree) = rotate @(Right Zig) tree >>= rotate @(Right Zig)

instance Rotatable (Left (Zig Zag)) (Construction Wye) where
	type Rotational (Left (Zig Zag)) (Construction Wye) a = Maybe (Construction Wye a)
	rotation (Tag tree) = rotate @(Left Zig) $ sub @Left %~ (TU . (>>= rotate @(Right Zig)) . run) $ tree

instance Rotatable (Right (Zig Zag)) (Construction Wye) where
	type Rotational (Right (Zig Zag)) (Construction Wye) a = Maybe (Construction Wye a)
	rotation (Tag tree) = rotate @(Right Zig) $ sub @Right %~ (TU . (>>= rotate @(Left Zig)) . run) $ tree
