{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Inventory (module Exports, Zoomable' (zoom'), Zoomable (Zooming, zoom_), zoom, overlook, (=<>), (~<>)) where

import Pandora.Paradigm.Inventory.Ability as Exports
import Pandora.Paradigm.Inventory.Some as Exports

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((#))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly), Simplification)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%), type (<--))
import Pandora.Paradigm.Primary.Algebraic (Pointable, extract)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (!))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))

instance Adjoint (->) (->) (Store s) (State s) where
	(-|) :: (Store s a -> b) -> a -> State s b
	f -| x = State ! \s -> (:*:) s . f . Store ! s :*: constant x
	(|-) :: (a -> State s b) -> Store s a -> b
	g |- Store (s :*: f) = extract . (run % s) . g ! f s

instance Adjoint (->) (->) (Accumulator e) (Imprint e) where
	f -| x = Imprint ! f . Accumulator -| x
	g |- x = run . g |- run x

instance Adjoint (->) (->) (Equipment e) (Provision e) where
	f -| x = Provision ! f . Equipment -| x
	g |- x = run . g |- run x

class Zoomable' (tool :: * -> * -> *) (available :: * -> *) where
	zoom' :: forall bg ls t . Adaptable t (->) (tool bg) => Lens available bg ls -> tool (Simplification available ls) ~> t

instance Zoomable' State Exactly where
	zoom' :: forall bg ls t result . Stateful bg t => Convex Lens bg ls -> State ls result -> t result
	zoom' lens less = adapt . State ! \source -> restruct |- run (lens ! source) where

		restruct :: (Exactly ls -> bg) -> Exactly ls -> bg :*: result
		restruct to (Exactly target) = run # to . Exactly <-|- Flip (less ! target)

instance Zoomable' State Maybe where
	zoom' :: forall bg ls t result . Stateful bg t => Obscure Lens bg ls -> State (Maybe ls) result -> t result
	zoom' lens less = adapt . State ! \source -> restruct |- run (lens ! source) where

		restruct :: (Maybe ls -> bg) -> Maybe ls -> bg :*: result
		restruct to target = run # to <-|- Flip (less ! target)

class Zoomable (available :: * -> *) where
	type Zooming available target :: *
	zoom_ :: forall bg ls t . Stateful bg t => Lens available bg ls -> State (Zooming available ls) ~> t

instance Zoomable Exactly where
	type Zooming Exactly target = target

	zoom_ :: forall bg ls t result . Stateful bg t => Convex Lens bg ls -> State ls result -> t result
	zoom_ lens less = adapt . State ! \source -> restruct |- run (lens ! source) where

		restruct :: (Exactly ls -> bg) -> Exactly ls -> bg :*: result
		restruct to (Exactly target) = run # to . Exactly <-|- Flip (less ! target)

instance Zoomable Maybe where
	type Zooming Maybe target = Maybe target

	zoom_ :: forall bg ls t result . Stateful bg t => Obscure Lens bg ls -> State (Maybe ls) result -> t result
	zoom_ lens less = adapt . State ! \source -> restruct |- run (lens ! source) where

		restruct :: (Maybe ls -> bg) -> Maybe ls -> bg :*: result
		restruct to target = run # to <-|- Flip (less ! target)

zoom :: forall bg ls t u result . Stateful bg t => Lens u bg ls -> State (u ls) result -> t result
zoom lens less = adapt . State ! \source -> restruct |- run (lens ! source) where

	restruct :: (u ls -> bg) -> u ls -> bg :*: result
	restruct to target = run # to <-|- Flip (less ! target)

overlook :: (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => State s result -> State (t s) (t result)
overlook (State state) = State ! \ts -> mult @(<--) @(:*:) @(:*:) ! (state <-|- ts)

(=<>) :: (Pointable available, Stateful src t)
	=> Lens available src tgt -> tgt -> t src
lens =<> new = adapt (modify @State ! set @(Lens _) new lens)

(~<>) :: (Pointable available, Covariant (->) (->) available, Gettable (Lens available), Stateful src t)
	=> Lens available src tgt -> (tgt -> tgt) -> t src
lens ~<> f = adapt (modify @State ! modify @(Lens _) f lens)
