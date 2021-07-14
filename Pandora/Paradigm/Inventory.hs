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
import Pandora.Pattern.Category ((.), ($), (#), identity)
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Adjoint (Adjoint_ ((--|-), (-|--)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Paradigm.Primary.Functor.Function ((!.), (%))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Controlflow.Effect.Adaptable (adapt)
import Pandora.Paradigm.Structure.Ability.Accessible (Accessible (access))

instance Adjoint_ (Store s) (State s) (->) (->) where
	(--|-) :: (Store s a -> b) -> a -> State s b
	f --|- x = State $ \s -> (:*:) s . f . Store $ s :*: (x !.)
	(-|--) :: (a -> State s b) -> Store s a -> b
	g -|-- Store (s :*: f) = extract . (run % s) . g $ f s

instance Adjoint_ (Accumulator e) (Imprint e) (->) (->) where
	f --|- x = Imprint $ f . Accumulator --|- x
	g -|-- x = run . g -|-- run x

instance Adjoint_ (Equipment e) (Environment e) (->) (->) where
	f --|- x = Environment $ f . Equipment --|- x
	g -|-- x = run . g -|-- run x

zoom :: Stateful bg t => Lens Identity bg ls -> State ls ~> t
zoom lens less = let restruct to = (to . Identity <-> identity) . run less . extract @Identity
	in adapt . State $ (restruct -|--) . run . run lens

(=<>) :: Stateful src t => Lens mode src tgt -> mode tgt -> t src
lens =<> new = modify $ set lens new

(~<>) :: Covariant mode => Stateful src t => Lens mode src tgt -> (mode tgt -> mode tgt) -> t src
lens ~<> f = modify $ over lens f

magnify :: forall bg ls t . (Accessible ls bg, Stateful bg t) => t ls
magnify = zoom @bg # access @ls @bg # current

adjust :: forall bg ls t . (Accessible ls bg, Stateful bg t) => (ls -> ls) -> t ls
adjust = zoom @bg (access @ls @bg) . modify
