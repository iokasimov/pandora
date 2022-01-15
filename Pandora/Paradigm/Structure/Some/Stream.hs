{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.Stream where

import Pandora.Core.Functor (type (:=), type (:=>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((#), (<---), (--->))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic (extract)
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Primary (twosome)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate), premorph, rotate)
import Pandora.Paradigm.Structure.Ability.Zipper (Zippable (Breadcrumbs), Tape)
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Primary.Algebraic (point)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (!))

type Stream = Construction Identity

instance Zippable (Construction Identity) where
	type Breadcrumbs (Construction Identity) = Reverse Stream <:.:> Stream := (:*:)

instance Morphable (Rotate Left) (Tape Stream) where
	type Morphing (Rotate Left) (Tape Stream) = Tape Stream
	morphing (run . premorph -> Identity x :*: T_U (Reverse bs :*: fs)) = twosome # Identity <--- extract bs
		! twosome # Reverse ---> extract <--- deconstruct bs # Construct x <--- point fs

instance Morphable (Rotate Right) (Tape Stream) where
	type Morphing (Rotate Right) (Tape Stream) = Tape Stream
	morphing (run . premorph -> Identity x :*: T_U (Reverse bs :*: fs)) = twosome # Identity (extract fs)
		! twosome # Reverse ---> Construct x <--- point bs # extract (deconstruct fs)

instance {-# OVERLAPS #-} Extendable (->) (Tape Stream) where
	f <<= z = let move rtt = extract . deconstruct ! point . rtt .-+ z in
		f <-|- twosome <--- Identity z <--- (twosome ! Reverse ---> move ---> rotate @Left ! move ---> rotate @Right)

repeat :: a :=> Stream
repeat x = Construct x . Identity ! repeat x
