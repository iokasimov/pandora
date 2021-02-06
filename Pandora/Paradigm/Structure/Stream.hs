{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Stream where

import Pandora.Core.Functor (type (:=>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant ((<$>))
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Primary.Functor.Delta (Delta ((:^:)))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), morph)
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

type Stream = Construction Identity

type instance Zipper Stream = Tap (Delta <:.> Stream)

instance Morphable Left (Tap (Delta <:.> Stream)) where
	type Morphing Left (Tap (Delta <:.> Stream)) = Tap (Delta <:.> Stream)
	morphing (extract . run -> Tap x (TU (bs :^: fs))) = Tap (extract bs) . TU
		$ extract (deconstruct bs) :^: Construct x (point fs)

instance Morphable Right (Tap (Delta <:.> Stream)) where
	type Morphing Right (Tap (Delta <:.> Stream)) = Tap (Delta <:.> Stream)
	morphing (extract . run -> Tap x (TU (bs :^: fs))) = Tap (extract fs) . TU
		$ Construct x (point bs) :^: extract (deconstruct fs)

instance {-# OVERLAPS #-} Extendable (Tap (Delta <:.> Stream)) where
	z =>> f = let move rtt = extract . deconstruct $ point . rtt .-+ z
		in f <$> Tap z (TU $ move (morph @Left) :^: move (morph @Right))

repeat :: a :=> Stream
repeat x = Construct x . Identity $ repeat x
