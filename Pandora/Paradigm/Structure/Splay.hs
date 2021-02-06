{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Pandora.Paradigm.Schemes (TU (TU))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), morph)
import Pandora.Paradigm.Structure.Ability.Substructure (substitute)
import Pandora.Paradigm.Structure.Binary (Binary)

data Splay a = Zig a | Zag a

instance Morphable (Left Zig) (Construction Wye) where
	type Morphing (Left Zig) (Construction Wye) = Binary
	morphing (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> left st where

		subtree = branches (deconstruct <$> left st >>= left) . Just . Construct parent
			$ branches (deconstruct <$> left st >>= right) (right st)

instance Morphable (Right Zig) (Construction Wye) where
	type Morphing (Right Zig) (Construction Wye) = Binary
	morphing (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> right st where

		subtree = branches (left st) . Just . Construct parent
			$ branches (deconstruct <$> right st >>= left) (deconstruct <$> right st >>= right)

instance Morphable (Left (Zig Zig)) (Construction Wye) where
	type Morphing (Left (Zig Zig)) (Construction Wye) = Binary
	morphing (extract . run -> tree) = TU $ run (morph @(Left Zig) tree) >>= run . morph @(Left Zig)

instance Morphable (Right (Zig Zig)) (Construction Wye) where
	type Morphing (Right (Zig Zig)) (Construction Wye) = Binary
	morphing (extract . run -> tree) = TU $ run (morph @(Right Zig) tree) >>= run . morph @(Right Zig)

instance Morphable (Left (Zig Zag)) (Construction Wye) where
	type Morphing (Left (Zig Zag)) (Construction Wye) = Binary
	morphing = morph @(Left Zig) . substitute @Left ((>>= run . morph @(Right Zig)) ||=) . extract . run

instance Morphable (Right (Zig Zag)) (Construction Wye) where
	type Morphing (Right (Zig Zag)) (Construction Wye) = Binary
	morphing = morph @(Right Zig) . substitute @Right ((>>= run . morph @(Left Zig)) ||=) . extract . run
