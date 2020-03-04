module Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic) where

type family Schematic (c :: (* -> *) -> k) (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
