{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Splay where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Morphism ((%))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Paradigm.Primary.Functor (left, right)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag), type (:#))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Inventory.Optics ((%~))
import Pandora.Paradigm.Structure.Ability.Rotatable (Rotatable (Rotational, rotation), rotate)
import Pandora.Paradigm.Structure.Ability.Substructure (sub)
import Pandora.Paradigm.Structure.Binary ()

data Splay a = Zig a | Zag a

instance Rotatable (Left Zig) (Construction Wye) where
	type Rotational (Left Zig) (Construction Wye) a = Maybe (Construction Wye a)
	rotation :: forall a . Left Zig :# Construction Wye a -> Maybe :. Construction Wye := a
	rotation (Tag (Construct parent st)) = Construct % subtree <$> found where

		found :: Maybe a
		found = extract <$> left st

		subtree :: Wye :. Construction Wye := a
		subtree = maybe_subtree (deconstruct <$> left st >>= left)
			. Just . Construct parent $ maybe_subtree
				(deconstruct <$> left st >>= right) (right st)

instance Rotatable (Right Zig) (Construction Wye) where
	type Rotational (Right Zig) (Construction Wye) a = Maybe (Construction Wye a)
	rotation :: forall a . Right Zig :# Construction Wye a -> Maybe :. Construction Wye := a
	rotation (Tag (Construct parent st)) = Construct % subtree <$> found where

		found :: Maybe a
		found = extract <$> right st

		subtree :: Wye :. Construction Wye := a
		subtree = maybe_subtree (left st)
			. Just . Construct parent $ maybe_subtree
				(deconstruct <$> right st >>= left)
				(deconstruct <$> right st >>= right)

instance Rotatable (Left (Zig Zig)) (Construction Wye) where
	type Rotational (Left (Zig Zig)) (Construction Wye) a = Maybe (Construction Wye a)
	rotation (Tag tree) = rotate @(Left Zig) tree >>= rotate @(Left Zig)

instance Rotatable (Right (Zig Zig)) (Construction Wye) where
	type Rotational (Right (Zig Zig)) (Construction Wye) a = Maybe (Construction Wye a)
	rotation (Tag tree) = rotate @(Right Zig) tree >>= rotate @(Right Zig)

instance Rotatable (Left (Zig Zag)) (Construction Wye) where
	type Rotational (Left (Zig Zag)) (Construction Wye) a = Maybe (Construction Wye a)
	rotation (Tag tree) = rotate @(Left Zig) $ sub @Left %~ (>>= rotate @(Right Zig)) $ tree

instance Rotatable (Right (Zig Zag)) (Construction Wye) where
	type Rotational (Right (Zig Zag)) (Construction Wye) a = Maybe (Construction Wye a)
	rotation (Tag tree) = rotate @(Right Zig) $ sub @Right %~ (>>= rotate @(Left Zig)) $ tree

maybe_subtree :: Maybe a -> Maybe a -> Wye a
maybe_subtree (Just x) (Just y) = Both x y
maybe_subtree Nothing (Just y) = Right y
maybe_subtree (Just x) Nothing = Left x
maybe_subtree Nothing Nothing = End
