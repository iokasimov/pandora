module Pandora.Paradigm.Structure.Ability.Zipper where

import Pandora.Core.Functor (type (:=), type (:::))
import Pandora.Core.Appliable ((!))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:)((:*:)))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Rotate))

class Zippable (structure :: * -> *) where
	type Breadcrumbs structure :: * -> *

type Zipper (structure :: * -> *) = Identity <:.:> Breadcrumbs structure := (:*:)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t => Semimonoidal (<--) (:*:) (:*:) (Identity <:.:> t := (:*:)) where
	mult = Flip $ \(T_U (Identity (x :*: y) :*: xys)) ->
		let xs :*: ys = mult @(<--) ! xys in
			T_U (Identity x :*: xs) :*: T_U (Identity y :*: ys)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t => Monoidal (<--) (->) (:*:) (:*:) (Identity <:.:> t := (:*:)) where
	unit _ = Flip $ \(T_U (Identity x :*: _)) -> (\_ -> x)

type family Fastenable structure rs where
	Fastenable structure (r ::: rs) = (Morphable (Rotate r) structure, Fastenable structure rs)
	Fastenable structure r = Morphable (Rotate r) structure
