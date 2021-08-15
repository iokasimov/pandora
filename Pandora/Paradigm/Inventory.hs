{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory (module Exports, zoom, magnify, (=<>), (~<>), adjust) where

import Pandora.Paradigm.Inventory.Optics as Exports
import Pandora.Paradigm.Inventory.Store as Exports
import Pandora.Paradigm.Inventory.State as Exports
import Pandora.Paradigm.Inventory.Imprint as Exports
import Pandora.Paradigm.Inventory.Equipment as Exports
import Pandora.Paradigm.Inventory.Environment as Exports
import Pandora.Paradigm.Inventory.Accumulator as Exports

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), (#), identity)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ((!.), (%))
import Pandora.Paradigm.Primary.Algebraic (extract_)
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Controlflow.Effect.Adaptable (adapt)
import Pandora.Paradigm.Structure.Ability.Accessible (Accessible (access))

instance Adjoint (Store s) (State s) (->) (->) where
	(-|) :: (Store s a -> b) -> a -> State s b
	f -| x = State $ \s -> (:*:) s . f . Store $ s :*: (x !.)
	(|-) :: (a -> State s b) -> Store s a -> b
	g |- Store (s :*: f) = extract . (run % s) . g $ f s

instance Adjoint (Accumulator e) (Imprint e) (->) (->) where
	f -| x = Imprint $ f . Accumulator -| x
	g |- x = run . g |- run x

instance Adjoint (Equipment e) (Environment e) (->) (->) where
	f -| x = Environment $ f . Equipment -| x
	g |- x = run . g |- run x

zoom :: Stateful bg t => Lens Identity bg ls -> State ls ~> t
zoom lens less = let restruct to = (to . Identity <-> identity @(->)) . run less . extract_ @Identity
	in adapt . State $ (restruct |-) . run . run lens

(=<>) :: Stateful src t => Lens mode src tgt -> mode tgt -> t src
lens =<> new = modify $ set lens new

(~<>) :: Stateful src t => Lens mode src tgt -> (mode tgt -> mode tgt) -> t src
lens ~<> f = modify $ over lens f

magnify :: forall bg ls t . (Accessible ls bg, Stateful bg t) => t ls
magnify = zoom @bg # access @ls @bg # current

adjust :: forall bg ls t . (Accessible ls bg, Stateful bg t) => (ls -> ls) -> t ls
adjust = zoom @bg (access @ls @bg) . modify
