{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.Splay where

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
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate), morph)
import Pandora.Paradigm.Structure.Ability.Substructure (substitute)
import Pandora.Paradigm.Structure.Some.Binary (Binary)

data Splay a = Zig a | Zag a

instance Morphable (Rotate (Left Zig)) (Construction Wye) where
	type Morphing (Rotate (Left Zig)) (Construction Wye) = Binary
	morphing (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> left st where

		subtree = branches (deconstruct <$> left st >>= left) . Just . Construct parent
			$ branches (deconstruct <$> left st >>= right) (right st)

instance Morphable (Rotate (Right Zig)) (Construction Wye) where
	type Morphing (Rotate (Right Zig)) (Construction Wye) = Binary
	morphing (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> right st where

		subtree = branches (left st) . Just . Construct parent
			$ branches (deconstruct <$> right st >>= left) (deconstruct <$> right st >>= right)

instance Morphable (Rotate (Left (Zig Zig))) (Construction Wye) where
	type Morphing (Rotate (Left (Zig Zig))) (Construction Wye) = Binary
	morphing (extract . run -> tree) = TU $ run (morph @(Rotate (Left Zig)) tree) >>= run . morph @(Rotate (Left Zig))

instance Morphable (Rotate (Right (Zig Zig))) (Construction Wye) where
	type Morphing (Rotate (Right (Zig Zig))) (Construction Wye) = Binary
	morphing (extract . run -> tree) = TU $ run (morph @(Rotate (Right Zig)) tree) >>= run . morph @(Rotate (Right Zig))

instance Morphable (Rotate (Left (Zig Zag))) (Construction Wye) where
	type Morphing (Rotate (Left (Zig Zag))) (Construction Wye) = Binary
	morphing = morph @(Rotate (Left Zig)) . substitute @Left ((>>= run . morph @(Rotate (Right Zig))) ||=) . extract . run

instance Morphable (Rotate (Right (Zig Zag))) (Construction Wye) where
	type Morphing (Rotate (Right (Zig Zag))) (Construction Wye) = Binary
	morphing = morph @(Rotate (Right Zig)) . substitute @Right ((>>= run . morph @(Rotate (Left Zig))) ||=) . extract . run
