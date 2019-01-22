module Pandora.Paradigm.Structure.Graph where

import Pandora.Core.Functor (type (:.:))
import Pandora.Paradigm.Basis.Edges (Edges)
import Pandora.Paradigm.Basis.Cofree (Cofree)

type Graph a = (Edges :.: Cofree Edges) a
