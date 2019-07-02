module Pandora.Paradigm.Structure.Graph (Graph, loose) where

import Pandora.Core.Functor (type (:.:))
import Pandora.Core.Morphism ((.))
import Pandora.Paradigm.Basis.Edges (Edges (Empty, Overlay))
import Pandora.Paradigm.Basis.Twister (Twister ((:<)))
import Pandora.Paradigm.Inventory.Stateful (fold)
import Pandora.Pattern.Functor.Traversable (Traversable)

-- | Acyclic graph structure without loops
type Graph a = (Edges :.: Twister Edges) a

-- | Transform any traversable structure into all loose edges graph
loose :: Traversable t => t a -> Graph a
loose = fold Empty (\x -> Overlay . (:<) x)
