module Pandora.Paradigm.Structure.Graph where

import Pandora.Paradigm.Basis.Edges (Edges)
import Pandora.Paradigm.Basis.Cofree (Cofree)
import Pandora.Paradigm.Basis.Junction.Transformer (type (:!:))

type Graph a = (Cofree Edges :!: Edges) a
