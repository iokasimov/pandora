{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.Stream where

import Pandora.Core.Functor (type (:=), type (:=>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((!), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)), twosome)
import Pandora.Paradigm.Primary.Algebraic (extract)
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate), premorph, rotate)
import Pandora.Paradigm.Structure.Ability.Zipper (Zippable (Breadcrumbs))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Primary.Algebraic (point)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)

type Stream = Construction Identity

instance Zippable (Construction Identity) where
	type Breadcrumbs (Construction Identity) = Stream <:.:> Stream := (:*:)

instance Morphable (Rotate Left) (Identity <:.:> (Stream <:.:> Stream := (:*:)) := (:*:)) where
	type Morphing (Rotate Left) (Identity <:.:> (Stream <:.:> Stream := (:*:)) := (:*:)) =
		Identity <:.:> (Stream <:.:> Stream := (:*:)) := (:*:)
	morphing (run . premorph -> Identity x :*: T_U (bs :*: fs)) = twosome # Identity (extract bs)
		! twosome # extract (deconstruct bs) # Construct x (point fs)

instance Morphable (Rotate Right) (Identity <:.:> (Stream <:.:> Stream := (:*:)) := (:*:)) where
	type Morphing (Rotate Right) (Identity <:.:> (Stream <:.:> Stream := (:*:)) := (:*:)) = Identity <:.:> (Stream <:.:> Stream := (:*:)) := (:*:)
	morphing (run . premorph -> Identity x :*: T_U (bs :*: fs)) = twosome # Identity (extract fs)
		! twosome # Construct x (point bs) # extract (deconstruct fs)

instance {-# OVERLAPS #-} Extendable (->) (Identity <:.:> (Stream <:.:> Stream := (:*:)) := (:*:)) where
	f <<= z = let move rtt = extract . deconstruct ! point . rtt .-+ z
		in f <-|- T_U (Identity z :*: twosome # move (rotate @Left) # move (rotate @Right))

repeat :: a :=> Stream
repeat x = Construct x . Identity ! repeat x
