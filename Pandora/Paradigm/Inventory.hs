{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Inventory (module Exports, zoom, overlook, (=<>), (~<>)) where

import Pandora.Paradigm.Inventory.Ability as Exports
import Pandora.Paradigm.Inventory.Some as Exports

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (--|), (|-), (|--)))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Exponential ((%), type (<--))
import Pandora.Paradigm.Algebraic (Pointable, extract)
import Pandora.Paradigm.Primary (Simplification)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (<~))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))

instance Adjoint (->) (->) (Store s) (State s) where
	(-|) :: (Store s a -> b) -> a -> State s b
	f -| x = State <-- \s -> (:*:) s . f . Store <--- s :*: constant x
	(|-) :: (a -> State s b) -> Store s a -> b
	g |- Store (s :*: f) = extract . (run % s) . g <-- f s

instance Adjoint (->) (->) (Accumulator e) (Imprint e) where
	f -| x = Imprint <--- f . Accumulator --| x
	g |- x = run . g |-- run x

instance Adjoint (->) (->) (Equipment e) (Provision e) where
	f -| x = Provision <--- f . Equipment --| x
	g |- x = run . g |-- run x

zoom :: forall bg ls t u result . Stateful bg t => Lens u bg ls -> State (u ls) result -> t result
zoom lens less = adapt . State <-- \source -> restruct |- run (lens <~ source) where

 	restruct :: (u ls -> bg) -> u ls -> bg :*: result
 	restruct to target = run @(->) <---- to <-|- Flip (less <~ target)

overlook :: (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => State s result -> State (t s) (t result)
overlook (State state) = State <-- \ts -> mult @(<--) @(:*:) @(:*:) <~ (state <-|- ts)

(=<>) :: (Pointable available, Stateful src t)
	=> Lens available src tgt -> tgt -> t src
lens =<> new = adapt (modify @State <-- set @(Lens _) new lens)

(~<>) :: (Pointable available, Covariant (->) (->) available, Gettable (Lens available), Stateful src t)
	=> Lens available src tgt -> (tgt -> tgt) -> t src
lens ~<> f = adapt (modify @State <-- modify @(Lens _) f lens)
