module Pandora.Paradigm.Structure.Property.Hollow (Hollow (..)) where

import Pandora.Paradigm.Basis.Cofree (Cofree)
import Pandora.Paradigm.Junction.Transformer (type (:>:))

class Hollow t where
	hollow :: r -> (Cofree t a -> r ) -> (Cofree :>: t) a -> r
