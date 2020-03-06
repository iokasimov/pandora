module Pandora.Paradigm.Structure.Specific.Graph (Graph, loose) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Transformation (type (~>))
import Pandora.Paradigm.Basis.Edges (Edges (Empty, Overlay))
import Pandora.Paradigm.Basis.Twister (Twister (Twister))
import Pandora.Paradigm.Inventory.State (fold)
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))

-- | Acyclic graph structure without loops
newtype Graph a = Graph (Edges :. Twister Edges := a)

instance Covariant Graph where
	f <$> Graph stack = Graph $ f <$$> stack

instance Traversable Graph where
	Graph stack ->> f = Graph <$> stack ->>> f

instance Interpreted Graph where
	type Primary Graph a = Edges :. Twister Edges := a
	unwrap (Graph stack) = stack

-- | Transform any traversable structure into all loose edges graph
loose :: Traversable t => t ~> Graph
loose = Graph . fold Empty (\x -> Overlay . Twister x)
