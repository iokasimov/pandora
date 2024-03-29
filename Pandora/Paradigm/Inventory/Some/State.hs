{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Inventory.Some.State where

import Pandora.Core.Functor (type (:.), type (>>>))
import Pandora.Core.Interpreted (Interpreted (Primary, run, (<~), unite, (=#-)))
import Pandora.Pattern.Morphism.Flip (Flip)
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Invariant (Invariant ((<!<)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<), (==<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Adjoint ((-|), (|-))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Inventory.Ability.Gettable (Gettable (Getting, get))
import Pandora.Paradigm.Inventory.Ability.Settable (Settable (Setting, set))
import Pandora.Paradigm.Inventory.Ability.Modifiable (Modifiable (Modification, modify))
import Pandora.Pattern.Operation.Exponential (type (-->))
import Pandora.Paradigm.Algebraic ((:*:) ((:*:)), delta)
import Pandora.Pattern.Operation.One (One (One))
import Pandora.Paradigm.Algebraic (Pointable, point, (<<-|-), (>-||-))
import Pandora.Paradigm.Schemes (Schematic, TUT (TUT), type (<:<.>:>))

-- | Effectful computation with a variable
newtype State s a = State ((->) s :. (:*:) s >>> a)

instance Covariant (->) (->) (State s) where
	f <-|- x = State <--- (<-|-) f . run x

instance Semimonoidal (-->) (:*:) (:*:) (State s) where
	mult = Straight <-- \(State g :*: State h) -> State <-- \s ->
		let old :*: x = g s in
		let new :*: y = h old in
		new :*: x :*: y

instance Monoidal (-->) (-->) (:*:) (:*:) (State s) where
	unit _ = Straight <-- State . (identity @(->) -|) . (<~ One)

instance Bindable (->) (State s) where
	f =<< x = State <---- (run . f |-) <-|- run x

instance Monad (->) (State s) where

instance Invariant (Flip State r) where
	f <!< g = (((g >-||-) . ((f <<-|-) <-|-) =#-) =#-)

instance Interpreted (->) (State s) where
	type Primary (State s) a = (->) s :. (:*:) s >>> a
	run ~(State x) = x
	unite = State

type instance Schematic Monad (State s) = (->) s <:<.>:> (:*:) s

instance Monadic (->) (State s) where
	wrap x = TM . TUT <---- point <-|- run x

type Stateful s t = Adaptable t (->) (State s)

current :: Stateful s t => t s
current = adapt <-- State delta

-- Modify the state, state is new value, return old value
change :: Stateful s t => (s -> s) -> t s
change f = adapt . State <-- \s -> f s :*: s

reconcile :: (Bindable (->) t, Stateful s t, Adaptable t (->) u) => (s -> u s) -> t s
reconcile f = adapt . set @State ==<< adapt . f ==<< adapt <-- get @State

type Memorable s t = (Covariant (->) (->) t, Pointable t, Stateful s t)

-- fold :: (Traversable (->) (->) t, Memorable s u) => (a -> s -> s) -> t a -> u s
-- fold op struct = adapt <-- get @State -*- (adapt . modify @State . op <-/- struct)

instance Gettable State where
	type Getting State state ouput = State state state
	get = State delta

instance Settable State where
	type Setting State state output = state -> State state state
	set new = State <--- \_ -> new :*: new

instance Modifiable State where
	type Modification State state output = (state -> state) -> State state state
	modify f = State <--- \s -> f s :*: f s
