module Pandora.Paradigm.Structure.Ability.Zipper where

import Pandora.Core.Functor (type (:=), type (:::))
import Pandora.Paradigm.Primary.Algebraic.Product (type (:*:))
import Pandora.Paradigm.Schemes (type (<:.:>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Rotate))

class Zippable (structure :: * -> *) where
	type Breadcrumbs structure :: * -> *

type Zipper (structure :: * -> *) = structure <:.:> Breadcrumbs structure := (:*:)

type family Fastenable structure rs where
	Fastenable structure (r ::: rs) = (Morphable (Rotate r) structure, Fastenable structure rs)
	Fastenable structure r = Morphable (Rotate r) structure
