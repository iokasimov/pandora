{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory (module Exports, zoom) where

import Pandora.Paradigm.Inventory.Optics as Exports
import Pandora.Paradigm.Inventory.Store as Exports
import Pandora.Paradigm.Inventory.State as Exports
import Pandora.Paradigm.Inventory.Imprint as Exports
import Pandora.Paradigm.Inventory.Equipment as Exports
import Pandora.Paradigm.Inventory.Environment as Exports
import Pandora.Paradigm.Inventory.Accumulator as Exports

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.), ($), identity)
import Pandora.Pattern.Functor (Adjoint ((-|), (|-)), extract, (<->))
import Pandora.Paradigm.Primary.Functor.Function ((!), (%))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Controlflow.Effect.Adaptable (adapt)

instance Adjoint (Store s) (State s) where
	(-|) :: a -> (Store s a -> b) -> State s b
	x -| f = State $ \s -> (:*:) s . f . Store $ s :*: (x !)
	(|-) :: Store s a -> (a -> State s b) -> b
	Store (s :*: f) |- g = extract . run % s . g $ f s

instance Adjoint (Accumulator e) (Imprint e) where
	x -| f = Imprint $ x -| f . Accumulator
	x |- g = run x |- run . g

instance Adjoint (Equipment e) (Environment e) where
	x -| f = Environment $ x -| f . Equipment
	x |- g = run x |- run . g

zoom :: Stateful bg t => Lens bg ls -> State ls ~> t
zoom lens less = let restruct f v = f <-> identity $ run less v
	in adapt . State $ (|- restruct) . run . lens
