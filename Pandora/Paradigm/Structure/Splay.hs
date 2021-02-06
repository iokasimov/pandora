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
import Pandora.Paradigm.Structure.Ability.Convertible (Convertible (Conversion, conversion), convert)
import Pandora.Paradigm.Structure.Ability.Substructure (substitute)
import Pandora.Paradigm.Structure.Binary (Binary)

data Splay a = Zig a | Zag a

instance Convertible (Left Zig) (Construction Wye) where
	type Conversion (Left Zig) (Construction Wye) = Binary
	conversion (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> left st where

		subtree = branches (deconstruct <$> left st >>= left) . Just . Construct parent
			$ branches (deconstruct <$> left st >>= right) (right st)

instance Convertible (Right Zig) (Construction Wye) where
	type Conversion (Right Zig) (Construction Wye) = Binary
	conversion (extract . run -> Construct parent st) = TU $ Construct % subtree . extract <$> right st where

		subtree = branches (left st) . Just . Construct parent
			$ branches (deconstruct <$> right st >>= left) (deconstruct <$> right st >>= right)

instance Convertible (Left (Zig Zig)) (Construction Wye) where
	type Conversion (Left (Zig Zig)) (Construction Wye) = Binary
	conversion (extract . run -> tree) = TU $ run (convert @(Left Zig) tree) >>= run . convert @(Left Zig)

instance Convertible (Right (Zig Zig)) (Construction Wye) where
	type Conversion (Right (Zig Zig)) (Construction Wye) = Binary
	conversion (extract . run -> tree) = TU $ run (convert @(Right Zig) tree) >>= run . convert @(Right Zig)

instance Convertible (Left (Zig Zag)) (Construction Wye) where
	type Conversion (Left (Zig Zag)) (Construction Wye) = Binary
	conversion = convert @(Left Zig) . substitute @Left ((>>= run . convert @(Right Zig)) ||=) . extract . run

instance Convertible (Right (Zig Zag)) (Construction Wye) where
	type Conversion (Right (Zig Zag)) (Construction Wye) = Binary
	conversion = convert @(Right Zig) . substitute @Right ((>>= run . convert @(Left Zig)) ||=) . extract . run
