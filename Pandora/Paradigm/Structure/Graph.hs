module Pandora.Paradigm.Structure.Graph (Graph, loose) where

import Pandora.Core.Morphism ((.))
import Pandora.Paradigm.Basis.Edges (Edges (Empty, Overlay))
import Pandora.Paradigm.Basis.Cofree (Cofree ((:<)))
import Pandora.Paradigm.Junction.Transformer (Y (Y), type (:>:))
import Pandora.Paradigm.Inventory.Stateful (fold)
import Pandora.Pattern.Functor.Traversable (Traversable)

-- | Acyclic graph structure without loops
type Graph a = (Cofree :>: Edges) a

-- | Transform any traversable structure into all loose edges graph
loose :: Traversable t => t a -> Graph a
loose = Y . fold Empty (\x -> Overlay . (:<) x)
