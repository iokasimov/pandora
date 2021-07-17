{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Some.Splay where

import Pandora.Core.Functor (type (~>), type (:.), type (:=))
import Pandora.Pattern.Category ((.), ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Paradigm.Primary ()
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Functor.Product (twosome)
import Pandora.Paradigm.Primary.Functor.Tagged (type (:#))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Functor ((-<*>-))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, unite)
import Pandora.Paradigm.Inventory.Optics (over)
import Pandora.Paradigm.Schemes (TU (TU), type (<:.>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate, Into), premorph, rotate, into)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Substructure (sub)
import Pandora.Paradigm.Structure.Some.Binary (Binary)

data Splay a = Zig a | Zag a

instance Morphable (Rotate (Left Zig)) Binary where
	type Morphing (Rotate (Left Zig)) Binary = Binary
	morphing (premorph -> binary) = unite $ run binary >>= run . rotate @(Left Zig)

instance Morphable (Rotate (Right Zig)) Binary where
	type Morphing (Rotate (Right Zig)) Binary = Binary
	morphing (premorph -> binary) = unite $ run binary >>= run . rotate @(Right Zig)

instance Morphable (Rotate (Left (Zig Zig))) Binary where
	type Morphing (Rotate (Left (Zig Zig))) Binary = Binary
	morphing (premorph -> binary) = unite $ run binary >>= run . rotate @(Left (Zig Zig))

instance Morphable (Rotate (Right (Zig Zig))) Binary where
	type Morphing (Rotate (Right (Zig Zig))) Binary = Binary
	morphing (premorph -> binary) = unite $ run binary >>= run . rotate @(Right (Zig Zig))

instance Morphable (Rotate (Left (Zig Zag))) Binary where
	type Morphing (Rotate (Left (Zig Zag))) Binary = Binary
	morphing (premorph -> binary) = unite $ run binary >>= run . rotate @(Left (Zig Zag))

instance Morphable (Rotate (Right (Zig Zag))) Binary where
	type Morphing (Rotate (Right (Zig Zag))) Binary = Binary
	morphing (premorph -> binary) = unite $ run binary >>= run . rotate @(Right (Zig Zag))

-------------------------------------- Non-empty Splay tree ----------------------------------------

instance Morphable (Rotate (Left Zig)) (Construction Wye) where
	type Morphing (Rotate (Left Zig)) (Construction Wye) = Binary
	morphing :: forall a . (:#) (Rotate (Left Zig)) <:.> Construction Wye := a -> Binary a
	morphing (premorph -> Construct x xs) = TU $ Construct -<$>- parent -<*>- Just nodes where

		nodes :: Wye :. Nonempty Binary := a
		nodes = into @Wye . twosome (branch @Left xs) . Just . Construct x
			. into @Wye $ twosome (deconstruct <$> branch @Right xs >>= branch @Left)
				(deconstruct <$> branch @Right xs >>= branch @Right)

		parent :: Maybe a
		parent = extract <$> branch @Right xs

instance Morphable (Rotate (Right Zig)) (Construction Wye) where
	type Morphing (Rotate (Right Zig)) (Construction Wye) = Binary
	morphing :: forall a . (:#) (Rotate (Right Zig)) <:.> Construction Wye := a -> Binary a
	morphing (premorph -> Construct x xs) = TU $ Construct -<$>- parent -<*>- Just nodes where

		nodes :: Wye :. Nonempty Binary := a
		nodes = into @Wye . twosome (deconstruct <$> branch @Left xs >>= branch @Left) . Just . Construct x
			. into @Wye $ twosome (deconstruct <$> branch @Left xs >>= branch @Right) # branch @Right xs

		parent :: Maybe a
		parent = extract <$> branch @Left xs

-- TODO: Morphing ... = Conclussion Error <:.> Nonempty Binary
instance Morphable (Rotate (Left (Zig Zig))) (Construction Wye) where
	type Morphing (Rotate (Left (Zig Zig))) (Construction Wye) = Binary
	morphing (premorph -> tree) = TU $ run # rotate @(Left Zig) tree >>= run . rotate @(Left Zig)

-- TODO: Morphing ... = Conclussion Error <:.> Nonempty Binary
instance Morphable (Rotate (Right (Zig Zig))) (Construction Wye) where
	type Morphing (Rotate (Right (Zig Zig))) (Construction Wye) = Binary
	morphing (premorph -> tree) = TU $ run # rotate @(Right Zig) tree >>= run . rotate @(Right Zig)

-- TODO: Morphing ... = Conclussion Error <:.> Nonempty Binary
instance Morphable (Rotate (Left (Zig Zag))) (Construction Wye) where
	type Morphing (Rotate (Left (Zig Zag))) (Construction Wye) = Binary
	morphing = rotate @(Left Zig) . over (sub @Left) (>>= run . rotate @(Right Zig)) . premorph

-- TODO: Morphing ... = Conclussion Error <:.> Nonempty Binary
instance Morphable (Rotate (Right (Zig Zag))) (Construction Wye) where
	type Morphing (Rotate (Right (Zig Zag))) (Construction Wye) = Binary
	morphing = rotate @(Right Zig) . over (sub @Right) (>>= run . rotate @(Left Zig)) . premorph

branch :: forall b . Morphable (Into (b Maybe)) Wye => Wye ~> Morphing (Into (b Maybe)) Wye
branch = into @(b Maybe)
