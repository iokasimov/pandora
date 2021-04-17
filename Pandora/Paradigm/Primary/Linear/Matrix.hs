module Pandora.Paradigm.Primary.Linear.Matrix where

import Pandora.Paradigm.Primary.Linear.Vector (Vector)

newtype Matrix i j a = Matrix (Vector i (Vector j a))
