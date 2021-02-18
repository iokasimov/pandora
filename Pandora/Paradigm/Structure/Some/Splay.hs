{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.Splay where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Paradigm.Primary ()
import Pandora.Paradigm.Primary.Functor (branches)
import Pandora.Paradigm.Primary.Functor.Function ((%))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (||=))
import Pandora.Paradigm.Schemes (TU (TU))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate), rotate, into)
import Pandora.Paradigm.Structure.Ability.Substructure (substitute)
import Pandora.Paradigm.Structure.Some.Binary (Binary)

data Splay a = Zig a | Zag a

instance Morphable (Rotate (Left Zig)) (Construction Wye) where
	type Morphing (Rotate (Left Zig)) (Construction Wye) = Binary
	morphing (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> into @(Left Maybe) st where

		subtree = branches (deconstruct <$> into @(Left Maybe) st >>= into @(Left Maybe)) . Just . Construct parent
			$ branches (deconstruct <$> into @(Left Maybe) st >>= into @(Right Maybe)) (into @(Right Maybe) st)

instance Morphable (Rotate (Right Zig)) (Construction Wye) where
	type Morphing (Rotate (Right Zig)) (Construction Wye) = Binary
	morphing (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> into @(Right Maybe) st where

		subtree = branches (into @(Left Maybe) st) . Just . Construct parent
			$ branches (deconstruct <$> into @(Right Maybe) st >>= into @(Left Maybe))
				(deconstruct <$> into @(Right Maybe) st >>= into @(Right Maybe))

instance Morphable (Rotate (Left (Zig Zig))) (Construction Wye) where
	type Morphing (Rotate (Left (Zig Zig))) (Construction Wye) = Binary
	morphing (extract . run -> tree) = TU $ run (rotate @(Left Zig) tree) >>= run . rotate @(Left Zig)

instance Morphable (Rotate (Right (Zig Zig))) (Construction Wye) where
	type Morphing (Rotate (Right (Zig Zig))) (Construction Wye) = Binary
	morphing (extract . run -> tree) = TU $ run (rotate @(Right Zig) tree) >>= run . rotate @(Right Zig)

instance Morphable (Rotate (Left (Zig Zag))) (Construction Wye) where
	type Morphing (Rotate (Left (Zig Zag))) (Construction Wye) = Binary
	morphing = rotate @(Left Zig) . substitute @Left ((>>= run . rotate @(Right Zig)) ||=) . extract . run

instance Morphable (Rotate (Right (Zig Zag))) (Construction Wye) where
	type Morphing (Rotate (Right (Zig Zag))) (Construction Wye) = Binary
	morphing = rotate @(Right Zig) . substitute @Right ((>>= run . rotate @(Left Zig)) ||=) . extract . run
