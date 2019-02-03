module Pandora.Paradigm.Structure.Concrete.Graph (Graph) where

import Pandora.Paradigm.Basis.Edges (Edges (Empty, Connect, Overlay))
import Pandora.Paradigm.Basis.Cofree (Cofree)
import Pandora.Paradigm.Junction.Transformer (Y (Y), type (:>:))
import Pandora.Paradigm.Structure.Property.Hollow (Hollow (hollow))

type Graph a = (Cofree :>: Edges) a

instance Hollow Edges where
	hollow result _ (Y Empty) = result
	hollow _ f (Y (Connect struct)) = f struct
	hollow _ f (Y (Overlay struct)) = f struct
