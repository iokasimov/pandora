{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Inventory (module Exports, zoom, magnify, overlook, probably, definitely, (=<>), (~<>)) where

import Pandora.Paradigm.Inventory.Ability as Exports
import Pandora.Paradigm.Inventory.Some as Exports

import Pandora.Core.Functor (type (>>>))
import Pandora.Core.Interpreted (run, (<~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (--|), (|-), (|--), (|---)))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly)
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Exponential ((%), type (<--))
import Pandora.Paradigm.Algebraic.Functor (Pointable, extract, (<<-|-))
import Pandora.Paradigm.Controlflow.Effect.Transformer ((:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Schemes.TUT (TUT (TUT))

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

-- TODO: Use <<-|- instead of run ... Flip <-- ..
zoom :: forall bg ls u result . Lens u bg ls -> State (u ls) result -> State bg result
zoom lens less = State <-- \source -> restruct |--- run <-- lens <~ source where

	restruct :: (u ls -> bg) -> u ls -> bg :*: result
	restruct to target = to <<-|- less <~ target

-- TODO: Use <<-|-|- instead of run <-|- ... Flip <-|- ..
magnify :: forall bg ls t u result . Covariant (->) (->) t
	=> Lens u bg ls -> State (u ls) :> t >>> result -> State bg :> t >>> result 
magnify lens less = TM . TUT <-- \source -> restruct |--- run <-- lens <~ source where

	restruct :: (u ls -> bg) -> u ls -> t (bg :*: result)
	restruct to target = (to <<-|-) <-|- less <~ target

overlook :: (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t) => State s result -> State (t s) (t result)
overlook (State state) = State <-- \ts -> mult @(<--) @(:*:) @(:*:) <~ (state <-|- ts)

-- TODO: it's better to rename it to `perhaps` but this name is already taken
-- `Perhaps` and `Accessibe` typeclasses should be generalized to only one
probably :: State s :> Maybe >>> result -> State s (Maybe result)
probably (TM (TUT state)) = State <-- \old -> case state old of
	Just (new :*: r) -> new :*: Just r
	Nothing -> old :*: Nothing

definitely :: State s :> Exactly >>> result -> State s result
definitely (TM (TUT state)) = State <-- extract . state

(=<>) :: (Pointable available, Stateful src t)
	=> Lens available src tgt -> tgt -> t src
lens =<> new = adapt (modify @State <-- set @(Lens _) new lens)

(~<>) :: (Pointable available, Covariant (->) (->) available, Gettable (Lens available), Stateful src t)
	=> Lens available src tgt -> (tgt -> tgt) -> t src
lens ~<> f = adapt (modify @State <-- modify @(Lens _) f lens)
