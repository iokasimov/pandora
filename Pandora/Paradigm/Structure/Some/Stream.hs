{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.Stream where

import Pandora.Core.Impliable (imply)
import Pandora.Core.Functor (type (>), type (:=>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (-->))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|--)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic (extract)
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate), premorph, rotate)
import Pandora.Paradigm.Structure.Ability.Zipper (Zippable (Breadcrumbs), Tape)
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Primary.Algebraic (point)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)

type Stream = Construction Exactly

instance Zippable (Construction Exactly) where
	type Breadcrumbs (Construction Exactly) = Reverse Stream <:.:> Stream > (:*:)

instance Morphable (Rotate Left) (Tape Stream) where
	type Morphing (Rotate Left) (Tape Stream) = Tape Stream
	morphing (run . premorph -> Exactly x :*: T_U (Reverse ls :*: rs)) =
		imply @(Tape Stream _) <--- extract ls <--- extract (deconstruct ls) <--- Construct x --> point rs

instance Morphable (Rotate Right) (Tape Stream) where
	type Morphing (Rotate Right) (Tape Stream) = Tape Stream
	morphing (run . premorph -> Exactly x :*: T_U (Reverse ls :*: rs)) =
		imply @(Tape Stream _) <--- extract rs <--- Construct x (point ls) <--- extract (deconstruct rs)

instance {-# OVERLAPS #-} Extendable (->) (Tape Stream) where
	f <<= z = let move rtt = extract . deconstruct <---- point . rtt .-+ z in
		f <-|-- imply @(Tape Stream _) <-- z <-- move (rotate @Left) <-- move (rotate @Right)

repeat :: a :=> Stream
repeat x = Construct x . Exactly <-- repeat x
