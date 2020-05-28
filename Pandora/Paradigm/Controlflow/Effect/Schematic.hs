module Pandora.Paradigm.Controlflow.Effect.Schematic (Schematic) where

type family Schematic (c :: (* -> *) -> k) (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
