{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.State where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Morphism ((%))
import Pandora.Pattern.Category (identity, (.), ($))
import Pandora.Pattern.Functor (Covariant ((<$>), (<$$>)), Avoidable (empty), Pointable (point), Applicative ((<*>), (*>)), Alternative ((<+>)), Traversable ((->>)), Bindable ((>>=), (>=>)), Monad, extract, (-|), (|-), (<*+>))
import Pandora.Paradigm.Controlflow (Adaptable (adapt), Interpreted (Primary, run), Monadic (lay, wrap), (:>) (TM), Schematic)
import Pandora.Paradigm.Schemes.TUT (TUT (TUT), type (<:<.>:>))
import Pandora.Paradigm.Primary.Functor (Predicate (Predicate), Product ((:*:)), type (:*:), delta)
import Pandora.Paradigm.Primary.Object (bool)

newtype State s a = State ((->) s :. (:*:) s := a)

instance Covariant (State s) where
	f <$> State x = State $ \old -> f <$> x old

instance Applicative (State s) where
	State f <*> State x = State $ \old ->
		let (new :*: g) = f old in g <$> x new

instance Pointable (State s) where
	point = State . (-| identity)

instance Bindable (State s) where
	State x >>= f = State $ \old ->
		(|- run) $ f <$> x old

instance Monad (State s) where

fold :: Traversable t => s -> (a -> s -> s) -> t a -> s
fold start op struct = extract . run @(State _) % start
	$ struct ->> modify . op *> current

find :: (Pointable u, Avoidable u, Alternative u, Traversable t) => Predicate a -> t a -> u a
find (Predicate p) = fold empty (\x s -> (<+>) s . bool empty (point x) . p $ x)

instance Interpreted (State s) where
	type Primary (State s) a = (->) s :. (:*:) s := a
	run (State x) = x

type instance Schematic Monad (State s) = (->) s <:<.>:> (:*:) s

instance Monadic (State s) where
	lay x = TM . TUT $ \s -> (s :*:) <$> x
	wrap x = TM . TUT $ point <$> run x

type Stateful s = Adaptable (State s)

instance Covariant u => Covariant ((->) s <:<.>:> (:*:) s := u) where
	f <$> TUT x = TUT $ (<$$>) f . x

instance Bindable u => Applicative ((->) s <:<.>:> (:*:) s := u) where
	TUT f <*> TUT x = TUT $ f >=> \(new :*: g) -> g <$$> x new

instance Pointable u => Pointable ((->) s <:<.>:> (:*:) s := u) where
	point = TUT . (-| point)

instance Bindable u => Bindable ((->) s <:<.>:> (:*:) s := u) where
	TUT x >>= f = TUT $ x >=> \(new :*: y) -> ($ new) . run . f $ y

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
