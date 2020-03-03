{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.State (State (..), Stateful, current, modify, replace, fold, find) where

import Pandora.Core.Functor (Variant (Co), type (:.), type (:=))
import Pandora.Core.Morphism ((%))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))
import Pandora.Paradigm.Controlflow.Joint.Transformer (Transformer (Schema, lay, wrap), (:>) (T))
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Schemes.TUV (TUV (TUV))
import Pandora.Paradigm.Basis.Predicate (Predicate (predicate))
import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:), attached, delta, uncurry)
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), ($>), (<$$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Setoid (bool)

newtype State s a = State ((->) s :. (:*:) s := a)

instance Covariant (State s) where
	f <$> State x = State $ \old -> f <$> x old

instance Applicative (State s) where
	State f <*> State x = State $ \old ->
		let latest = attached . x $ old in
			latest :*: (extract (f old) . extract . x $ old)

instance Pointable (State s) where
	point x = State $ \s -> s :*: x

instance Bindable (State s) where
	State x >>= f = State $ \old ->
		uncurry (unwrap %) $ f <$> x old

instance Monad (State s) where

fold :: Traversable t => s -> (a -> s -> s) -> t a -> s
fold start op struct = extract . unwrap @(State _) % start $
	struct ->> modify . op $> () *> current

find :: (Pointable u, Avoidable u, Alternative u, Traversable t) => Predicate a -> t a -> u a
find p struct = fold empty (\x s -> (<+>) s . bool empty (point x) . predicate p $ x) struct

instance Interpreted (State s) where
	type Primary (State s) a = (->) s :. (:*:) s := a
	unwrap (State x) = x

instance Transformer (State s) where
	type Schema (State s) u = TUV 'Co 'Co 'Co ((->) s) u ((:*:) s)
	lay x = T . TUV $ \s -> (s :*:) <$> x
	wrap x = T . TUV $ point <$> unwrap x

type Stateful s = Adaptable (State s)

instance Covariant u => Covariant (TUV 'Co 'Co 'Co ((->) s) u ((:*:) s)) where
	f <$> TUV x = TUV $ \old -> f <$$> x old

instance Bindable u => Applicative (TUV 'Co 'Co 'Co ((->) s) u ((:*:) s)) where
	TUV f <*> TUV x = TUV $ \old -> f old >>= \(new :*: g) -> g <$$> x new

instance Pointable u => Pointable (TUV 'Co 'Co 'Co ((->) s) u ((:*:) s)) where
	point x = TUV $ \s -> point $ s :*: x

instance Bindable u => Bindable (TUV 'Co 'Co 'Co ((->) s) u ((:*:) s)) where
	TUV x >>= f = TUV $ \old -> x old >>= \(new :*: y) -> ($ new) . unwrap . f $ y

instance Monad u => Monad (TUV 'Co 'Co 'Co ((->) s) u ((:*:) s)) where

current :: (Covariant t, Stateful s t) => t s
current = adapt $ State delta

modify :: (Covariant t, Stateful s t) => (s -> s) -> t ()
modify f = adapt $ State $ \s -> f s :*: ()

replace :: (Covariant t, Stateful s t) => s -> t ()
replace s = adapt $ State $ \_ -> s :*: ()
