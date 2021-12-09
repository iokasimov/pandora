{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory (module Exports, zoom, overlook, (=<>), (~<>)) where

import Pandora.Paradigm.Inventory.Optics as Exports
import Pandora.Paradigm.Inventory.Store as Exports
import Pandora.Paradigm.Inventory.State as Exports
import Pandora.Paradigm.Inventory.Imprint as Exports
import Pandora.Paradigm.Inventory.Equipment as Exports
import Pandora.Paradigm.Inventory.Environment as Exports
import Pandora.Paradigm.Inventory.Accumulator as Exports

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ((!.), (%), type (<--))
import Pandora.Paradigm.Primary.Algebraic (extract)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (!))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))

instance Adjoint (->) (->) (Store s) (State s) where
	(-|) :: (Store s a -> b) -> a -> State s b
	f -| x = State $ \s -> (:*:) s . f . Store $ s :*: (x !.)
	(|-) :: (a -> State s b) -> Store s a -> b
	g |- Store (s :*: f) = extract . (run % s) . g $ f s

instance Adjoint (->) (->) (Accumulator e) (Imprint e) where
	f -| x = Imprint $ f . Accumulator -| x
	g |- x = run . g |- run x

instance Adjoint (->) (->) (Equipment e) (Environment e) where
	f -| x = Environment $ f . Equipment -| x
	g |- x = run . g |- run x

zoom :: forall bg ls t u result . Stateful bg t => Lens u bg ls -> State (u ls) result -> t result
zoom lens less = adapt . State $ \source -> restruct |- run (lens ! source) where

	restruct :: (u ls -> bg) -> u ls -> bg :*: result
	restruct to target = run $ to <-|- Flip (less ! target)

overlook :: (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => State s result -> State (t s) (t result)
overlook (State state) = State $ \ts -> mult @(<--) @(:*:) @(:*:) ! (state <-|- ts)

(=<>) :: Stateful src t => Lens available src tgt -> available tgt -> t src
lens =<> new = modify $ set lens new

(~<>) :: Stateful src t => Lens available src tgt -> (available tgt -> available tgt) -> t src
lens ~<> f = modify $ over lens f
