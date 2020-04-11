{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Graph (Graph, loose) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Paradigm.Basis.Edges (Edges (Empty, Overlay))
import Pandora.Paradigm.Basis.Product (type (:*:))
import Pandora.Paradigm.Basis.Twister (Twister (Twister))
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Paradigm.Inventory.State (fold)
import Pandora.Paradigm.Structure.Variation.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Variation.Weighted (Weighted)

-- | Directed acyclic graph structure
type Graph = UT Covariant Covariant (Twister Edges) Edges

type instance Nonempty Graph = Twister Edges
type instance Weighted Graph w a = Twister Edges (w :*: a)

instance Covariant Graph where
	f <$> UT g = UT $ f <$$> g

instance Traversable Graph where
	UT g ->> f = UT <$> g ->>> f

-- | Transform any traversable structure into all loose edges graph
loose :: Traversable t => t ~> Graph
loose = UT . fold Empty (\x -> Overlay . Twister x)
