module Pandora.Paradigm.Structure.Concrete.Graph (Graph, loose) where

import Pandora.Core.Morphism ((.))
import Pandora.Paradigm.Basis.Edges (Edges (Empty, Connect, Overlay))
import Pandora.Paradigm.Basis.Cofree (Cofree ((:<)))
import Pandora.Paradigm.Junction.Transformer (Y (Y), type (:>:))
import Pandora.Paradigm.Inventory.Stateful (fold)
import Pandora.Paradigm.Structure.Property.Hollow (Hollow (hollow))
import Pandora.Pattern.Functor.Traversable (Traversable)

-- | Acyclic graph structure without loops
type Graph a = (Cofree :>: Edges) a

instance Hollow Edges where
	hollow result _ (Y Empty) = result
	hollow _ f (Y (Connect struct)) = f struct
	hollow _ f (Y (Overlay struct)) = f struct

-- | Transform any traversable structure into all loose edges graph
loose :: Traversable t => t a -> Graph a
loose = Y . fold Empty (\x -> Overlay . (:<) x)
