{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.State (State (..), Stateful, current, modify, replace, fold, find) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Morphism ((%))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), ($>), (<$$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Adjoint ((|-))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Joint.Transformer.Monadic (Monadic (lay, wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Schemes.TUT (TUT (TUT))
import Pandora.Paradigm.Primary.Object.Boolean (bool)
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), delta)

newtype State s a = State ((->) s :. (:*:) s := a)

instance Covariant (State s) where
	f <$> State x = State $ \old -> f <$> x old

instance Applicative (State s) where
	State f <*> State x = State $ \old ->
		let (new :*: g) = f old in g <$> x new

instance Pointable (State s) where
	point x = State $ \s -> s :*: x

instance Bindable (State s) where
	State x >>= f = State $ \old ->
		(|- run) $ f <$> x old

instance Monad (State s) where

fold :: Traversable t => s -> (a -> s -> s) -> t a -> s
fold start op struct = extract . run @(State _) % start $
	struct ->> modify . op $> () *> current

find :: (Pointable u, Avoidable u, Alternative u, Traversable t) => Predicate a -> t a -> u a
find p struct = fold empty (\x s -> (<+>) s . bool empty (point x) . predicate p $ x) struct

instance Interpreted (State s) where
	type Primary (State s) a = (->) s :. (:*:) s := a
	run (State x) = x

type instance Schematic Monad (State s) u =
	TUT Covariant Covariant Covariant ((->) s) ((:*:) s) u

instance Monadic (State s) where
	lay x = TM . TUT $ \s -> (s :*:) <$> x
	wrap x = TM . TUT $ point <$> run x

type Stateful s = Adaptable (State s)

instance Covariant u => Covariant (TUT Covariant Covariant Covariant ((->) s) ((:*:) s) u) where
	f <$> TUT x = TUT $ \old -> f <$$> x old

instance Bindable u => Applicative (TUT Covariant Covariant Covariant ((->) s) ((:*:) s) u) where
	TUT f <*> TUT x = TUT $ \old -> f old >>= \(new :*: g) -> g <$$> x new

instance Pointable u => Pointable (TUT Covariant Covariant Covariant ((->) s) ((:*:) s) u) where
	point x = TUT $ \s -> point $ s :*: x

instance Bindable u => Bindable (TUT Covariant Covariant Covariant ((->) s) ((:*:) s) u) where
	TUT x >>= f = TUT $ \old -> x old >>= \(new :*: y) -> ($ new) . run . f $ y

instance Monad u => Monad (TUT Covariant Covariant Covariant ((->) s) ((:*:) s) u) where

current :: Stateful s t => t s
current = adapt $ State delta

modify :: Stateful s t => (s -> s) -> t ()
modify f = adapt $ State $ \s -> f s :*: ()

replace :: Stateful s t => s -> t ()
replace s = adapt $ State $ \_ -> s :*: ()
