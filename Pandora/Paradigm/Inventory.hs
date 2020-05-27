{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory (module Exports) where

import Pandora.Paradigm.Inventory.Optics as Exports
import Pandora.Paradigm.Inventory.Store as Exports
import Pandora.Paradigm.Inventory.State as Exports
import Pandora.Paradigm.Inventory.Imprint as Exports
import Pandora.Paradigm.Inventory.Equipment as Exports
import Pandora.Paradigm.Inventory.Environment as Exports
import Pandora.Paradigm.Inventory.Accumulator as Exports

import Pandora.Core.Morphism ((!), (%))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (run)

instance Adjoint (Store s) (State s) where
	x -| f = State $ \s -> (:*:) s . f . Store $ s :*: (x !)
	Store (s :*: f) |- g = extract . run % s . g $ f s

instance Adjoint (Accumulator e) (Imprint e) where
	x -| f = Imprint $ x -| f . Accumulator
	x |- g = run x |- run . g

instance Adjoint (Equipment e) (Environment e) where
	x -| f = Environment $ x -| f . Equipment
	x |- g = run x |- run . g
