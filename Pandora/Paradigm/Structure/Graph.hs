module Pandora.Paradigm.Structure.Graph (Graph) where

import Pandora.Core.Functor (type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Edges (Edges (Empty, Overlay))
import Pandora.Paradigm.Basis.Twister (Twister ((:<)))
import Pandora.Pattern.Junction.Composition (Composition (Primary, unwrap))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))

-- | Acyclic graph structure without loops
newtype Graph a = Graph (Edges :.: Twister Edges >< a)

instance Covariant Graph where
	f <$> Graph stack = Graph $ f <$$> stack

instance Composition Graph where
	type Primary Graph a = Edges :.: Twister Edges >< a
	unwrap (Graph stack) = stack
