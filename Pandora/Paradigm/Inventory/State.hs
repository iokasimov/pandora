{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.State where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category (identity, (.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Adjoint ((-|), (|-), ($|-))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite), Schematic)
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Schemes.TUT (TUT (TUT), type (<:<.>:>))
import Pandora.Paradigm.Primary.Functor (Product ((:*:)), type (:*:), delta)

-- | Effectful computation with a variable
newtype State s a = State ((->) s :. (:*:) s := a)

instance Covariant (State s) where
	f <$> x = State $ (<$>) f . run x

instance Applicative (State s) where
	f <*> x = State $ (|- (<$>)) . (run x <-> identity) . run f

instance Pointable (State s) where
	point = State . (-| identity)

instance Bindable (State s) where
	x >>= f = State $ run x $|- run . f

instance Monad (State s) where

instance Interpreted (State s) where
	type Primary (State s) a = (->) s :. (:*:) s := a
	run ~(State x) = x
	unite = State

type instance Schematic Monad (State s) = (->) s <:<.>:> (:*:) s

instance Monadic (State s) where
	wrap x = TM . TUT $ point <$> run x

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

type Memorable s t = (Pointable t, Applicative t, Stateful s t)

fold :: (Traversable t, Memorable s u) => (a -> s -> s) -> t a -> u s
fold op struct = (struct ->> modify . op) *> current
