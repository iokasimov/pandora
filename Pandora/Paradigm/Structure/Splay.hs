{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Splay where

import Pandora.Core.Morphism ((%))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Paradigm.Primary.Functor (left, right)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Inventory.Optics ((%~))
import Pandora.Paradigm.Structure.Binary ()
import Pandora.Paradigm.Structure.Ability.Rotatable (Rotatable (rotation), rotate)
import Pandora.Paradigm.Structure.Ability.Substructure (sub)

data Splay a = Zig a | Zag a

instance Rotatable (Left Zig) (Construction Wye) where
	rotation (Tag (Construct parent st)) = Construct % subtree <$> found where

		subtree = maybe_subtree a . Just . Construct parent $ maybe_subtree b c
		found = extract <$> left st
		a = deconstruct <$> left st >>= left
		b = deconstruct <$> left st >>= right
		c = right st

instance Rotatable (Right Zig) (Construction Wye) where
	rotation (Tag (Construct parent st)) = Construct % subtree <$> found where

		found = extract <$> right st
		subtree = maybe_subtree a . Just . Construct parent $ maybe_subtree b c
		a = left st
		b = deconstruct <$> right st >>= left
		c = deconstruct <$> right st >>= right

instance Rotatable (Left (Zig Zig)) (Construction Wye) where
	rotation (Tag tree) = rotate @(Left Zig) tree >>= rotate @(Left Zig)

instance Rotatable (Right (Zig Zig)) (Construction Wye) where
	rotation (Tag tree) = rotate @(Right Zig) tree >>= rotate @(Right Zig)

instance Rotatable (Left (Zig Zag)) (Construction Wye) where
	rotation (Tag tree) = rotate @(Left Zig)
		$ sub @Left %~ (>>= rotate @(Right Zig)) $ tree

instance Rotatable (Right (Zig Zag)) (Construction Wye) where
	rotation (Tag tree) = rotate @(Right Zig)
		$ sub @Right %~ (>>= rotate @(Left Zig)) $ tree

maybe_subtree :: Maybe a -> Maybe a -> Wye a
maybe_subtree (Just x) (Just y) = Both x y
maybe_subtree Nothing (Just y) = Right y
maybe_subtree (Just x) Nothing = Left x
maybe_subtree Nothing Nothing = End
