{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.Stream where

import Pandora.Core.Functor (type (:=), type (:=>), type (:::))
import Pandora.Pattern.Category ((.), ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), twosome)
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate), premorph, rotate)
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))

type Stream = Construction Identity

type instance Zipper (Construction Identity) (Left ::: Right) = Tap (Stream <:.:> Stream := (:*:))

instance Morphable (Rotate Left) (Tap (Stream <:.:> Stream := (:*:))) where
	type Morphing (Rotate Left) (Tap (Stream <:.:> Stream := (:*:))) = Tap (Stream <:.:> Stream := (:*:))
	morphing (premorph -> Tap x (T_U (bs :*: fs))) = Tap # extract bs
		$ twosome # extract (deconstruct bs) # Construct x (point fs)

instance Morphable (Rotate Right) (Tap (Stream <:.:> Stream := (:*:))) where
	type Morphing (Rotate Right) (Tap (Stream <:.:> Stream := (:*:))) = Tap (Stream <:.:> Stream := (:*:))
	morphing (premorph -> Tap x (T_U (bs :*: fs))) = Tap # extract fs
		$ twosome # Construct x (point bs) # extract (deconstruct fs)

instance {-# OVERLAPS #-} Extendable (Tap (Stream <:.:> Stream := (:*:))) where
	z =>> f = let move rtt = extract . deconstruct $ point . rtt .-+ z
		in f <$> Tap z (twosome # (move $ rotate @Left) # (move $ rotate @Right))

repeat :: a :=> Stream
repeat x = Construct x . Identity $ repeat x
