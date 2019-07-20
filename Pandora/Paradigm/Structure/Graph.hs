module Pandora.Paradigm.Structure.Graph (Graph, loose) where

import Pandora.Core.Functor (type (:.:), type (>))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Edges (Edges (Empty, Overlay))
import Pandora.Paradigm.Basis.Twister (Twister ((:<)))
import Pandora.Paradigm.Inventory.Stateful (fold)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Junction.Composition (Composition (Primary, unwrap))

-- | Acyclic graph structure without loops
newtype Graph a = Graph (Edges :.: Twister Edges > a)

instance Covariant Graph where
	f <$> Graph stack = Graph $ f <$$> stack

instance Traversable Graph where
	Graph stack ->> f = Graph <$> stack ->>> f

instance Composition Graph where
	type Primary Graph a = Edges :.: Twister Edges > a
	unwrap (Graph stack) = stack

-- | Transform any traversable structure into all loose edges graph
loose :: Traversable t => t a -> Graph a
loose = Graph . fold Empty (\x -> Overlay . (:<) x)
