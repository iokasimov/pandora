{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.State where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category (identity, (.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=), (>=>)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Adjoint ((-|), (|-))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Pattern.Functor ((<*+>))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run), Schematic)
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Schemes.TUT (TUT (TUT), type (<:<.>:>))
import Pandora.Paradigm.Primary.Functor (Predicate, Product ((:*:)), type (:*:), delta, satisfy)

newtype State s a = State ((->) s :. (:*:) s := a)

instance Covariant (State s) where
	f <$> State x = State $ \old -> f <$> x old

instance Applicative (State s) where
	State f <*> State x = State $
		(|- (<$>)) . (x <-> identity) . f

instance Pointable (State s) where
	point = State . (-| identity)

instance Bindable (State s) where
	State x >>= f = State $ \old ->
		(|- run) $ f <$> x old

instance Monad (State s) where

instance Interpreted (State s) where
	type Primary (State s) a = (->) s :. (:*:) s := a
	run (State x) = x

type instance Schematic Monad (State s) = (->) s <:<.>:> (:*:) s

instance Monadic (State s) where
	wrap x = TM . TUT $ point <$> run x

type Stateful s = Adaptable (State s)

instance Covariant u => Covariant ((->) s <:<.>:> (:*:) s := u) where
	f <$> TUT x = TUT $ (<$$>) f . x

instance Bindable u => Applicative ((->) s <:<.>:> (:*:) s := u) where
	TUT f <*> TUT x = TUT $ f >=> \ ~(new :*: g) -> g <$$> x new

instance Pointable u => Pointable ((->) s <:<.>:> (:*:) s := u) where
	point = TUT . (-| point)

instance Bindable u => Bindable ((->) s <:<.>:> (:*:) s := u) where
	TUT x >>= f = TUT $ x >=> \ ~(new :*: y) -> ($ new) . run . f $ y

instance Monad u => Monad ((->) s <:<.>:> (:*:) s := u) where

instance Alternative u => Alternative ((->) s <:<.>:> (:*:) s := u) where
	TUT x <+> TUT y = TUT (x <*+> y)

instance Avoidable u => Avoidable ((->) s <:<.>:> (:*:) s := u) where
	empty = TUT $ \_ -> empty

current :: Stateful s t => t s
current = adapt $ State delta

modify :: Stateful s t => (s -> s) -> t ()
modify f = adapt . State $ (:*: ()) . f

replace :: Stateful s t => s -> t ()
replace s = adapt . State $ \_ -> s :*: ()

type Memorable s t = (Pointable t, Applicative t, Stateful s t)

fold :: (Traversable t, Memorable s u) => (a -> s -> s) -> t a -> u s
fold op struct = struct ->> modify . op *> current

type Decisive t = (Pointable t, Avoidable t, Alternative t, Applicative t)

find :: (Traversable t, Decisive u, Memorable (u a) v) => Predicate a -> t a -> v (u a)
find p = fold (\x s -> s <+> satisfy p x)
