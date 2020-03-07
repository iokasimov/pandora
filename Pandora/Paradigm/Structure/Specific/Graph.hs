{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Specific.Graph (Graph, loose) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Paradigm.Basis.Edges (Edges (Empty, Overlay))
import Pandora.Paradigm.Basis.Twister (Twister (Twister))
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Paradigm.Inventory.State (fold)

-- | Directed acyclic graph structure
type Graph = UT Covariant Covariant (Twister Edges) Edges

instance Covariant (UT Covariant Covariant (Twister Edges) Edges) where
	f <$> UT g = UT $ f <$$> g

instance Traversable (UT Covariant Covariant (Twister Edges) Edges) where
	UT g ->> f = UT <$> g ->>> f

-- | Transform any traversable structure into all loose edges graph
loose :: Traversable t => t ~> Graph
loose = UT . fold Empty (\x -> Overlay . Twister x)
