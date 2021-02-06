{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Stream where

import Pandora.Core.Functor (type (:=>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), morph)
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))

type Stream = Construction Identity

type instance Zipper Stream = Tap ((:*:) <:.:> Stream)

instance Morphable Left (Tap ((:*:) <:.:> Stream)) where
	type Morphing Left (Tap ((:*:) <:.:> Stream)) = Tap ((:*:) <:.:> Stream)
	morphing (extract . run -> Tap x (T_U (bs :*: fs))) = Tap (extract bs) . T_U
		$ extract (deconstruct bs) :*: Construct x (point fs)

instance Morphable Right (Tap ((:*:) <:.:> Stream)) where
	type Morphing Right (Tap ((:*:) <:.:> Stream)) = Tap ((:*:) <:.:> Stream)
	morphing (extract . run -> Tap x (T_U (bs :*: fs))) = Tap (extract fs) . T_U
		$ Construct x (point bs) :*: extract (deconstruct fs)

instance {-# OVERLAPS #-} Extendable (Tap ((:*:) <:.:> Stream)) where
	z =>> f = let move rtt = extract . deconstruct $ point . rtt .-+ z
		in f <$> Tap z (T_U $ move (morph @Left) :*: move (morph @Right))

repeat :: a :=> Stream
repeat x = Construct x . Identity $ repeat x
