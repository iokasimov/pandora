{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Stream where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Paradigm.Primary.Functor.Delta (Delta ((:^:)))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Structure.Ability.Rotatable (Rotatable (Rotational, rotation))
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

type Stream = Construction Identity

type instance Zipper Stream = Tap (Delta <:.> Stream)

instance Rotatable Left (Tap (Delta <:.> Stream)) where
	type Rotational Left (Tap (Delta <:.> Stream)) a = Tap (Delta <:.> Stream) a
	rotation (extract -> Tap x (TU (bs :^: fs))) = Tap (extract bs) . TU
		$ extract (deconstruct bs) :^: Construct x (point fs)

instance Rotatable Right (Tap (Delta <:.> Stream)) where
	type Rotational Right (Tap (Delta <:.> Stream)) a = Tap (Delta <:.> Stream) a
	rotation (extract -> Tap x (TU (bs :^: fs))) = Tap (extract fs) . TU
		$ Construct x (point bs) :^: extract (deconstruct fs)

repeat :: a -> Stream a
repeat x = Construct x . Identity $ repeat x

iterate :: (a -> a) -> a -> Stream a
iterate f x = Construct x . Identity $ iterate f (f x)
