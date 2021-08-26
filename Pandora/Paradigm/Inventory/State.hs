{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.State where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Invariant (Invariant ((<$<)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Adjoint ((-|), (|-))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Pattern.Functor.Divariant ((>->))
import Pandora.Paradigm.Primary.Transformer (Flip)
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (||=)), Schematic)
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Schemes.TUT (TUT (TUT), type (<:<.>:>))
import Pandora.Paradigm.Primary.Algebraic ((:*:) ((:*:)), (*>-), delta)
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (point)

-- | Effectful computation with a variable
newtype State s a = State ((->) s :. (:*:) s := a)

instance Covariant (->) (->) (State s) where
	f -<$>- x = State $ (-<$>-) f . run x

instance Semimonoidal (->) (:*:) (:*:) (State s) where
	multiply (State g :*: State h) = State $ \s -> 
		let old :*: x = g s in 
	  	let new :*: y = h old in
		new :*: x :*: y

instance Monoidal (->) (->) (:*:) (:*:) (State s) where
	unit _ f = State . (identity @(->) -|) $ f One

instance Bindable (->) (State s) where
	f =<< x = State $ (run . f |-) -<$>- run x

instance Monad (State s) where

instance Invariant (Flip State r) where
	f <$< g = ((g >-> ((<->) @_ @(->) @(->) f identity) ||=) ||=)

instance Interpreted (State s) where
	type Primary (State s) a = (->) s :. (:*:) s := a
	run ~(State x) = x
	unite = State

type instance Schematic Monad (State s) = (->) s <:<.>:> (:*:) s

instance Monadic (State s) where
	wrap x = TM . TUT $ point -<$>- run x

type Stateful s = Adaptable (State s)

-- | Get current value
current :: Stateful s t => t s
current = adapt $ State delta

-- | Modify stored value with a function
modify :: Stateful s t => (s -> s) -> t s
modify f = adapt . State $ \s -> let r = f s in r :*: r

-- | Replace current value with another one
replace :: Stateful s t => s -> t s
replace s = adapt . State $ \_ -> s :*: s

reconcile :: (Bindable (->) t, Stateful s t, Adaptable u t) => (s -> u s) -> t s
reconcile f = replace =<< adapt . f =<< current

type Memorable s t = (Covariant (->) (->) t, Monoidal (->) (->) (:*:) (:*:) t,  Stateful s t)

fold :: (Traversable (->) (->) t, Memorable s u) => (a -> s -> s) -> t a -> u s
fold op struct = (modify . op <<- struct) *>- current
