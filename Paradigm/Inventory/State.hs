module Paradigm.Inventory.State (Stateful (..), State, get, modify, put) where

import Core.Composition ((:.:))
import Core.Morphism ((.), ($))
import Paradigm.Basis.Identity (Identity)
import Paradigm.Basis.Product (Product ((:&:)), type (:&:), delta)
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Applicative (Applicative ((<*>)))
import Pattern.Functor.Bindable (Bindable ((>>=)))
import Pattern.Functor.Monad (Monad)
import Pattern.Functor.Liftable (Liftable (lift))

newtype Stateful s t a = Stateful { statefully :: ((->) s :.: t :.: (:&:) s) a }

type State s = Stateful s Identity

instance Covariant t => Covariant (Stateful s t) where
	f <$> Stateful x = Stateful $ \old -> (comap . comap) f $ x old

instance Monad t => Applicative (Stateful s t) where
	Stateful f <*> Stateful x = Stateful $ \old ->
		f old >>= \(new :&: g) -> comap g <$> x new

instance Pointable t => Pointable (Stateful s t) where
	point x = Stateful $ \s -> point $ s :&: x

instance Bindable t => Bindable (Stateful s t) where
	Stateful x >>= f = Stateful $ \old ->
		x old >>= \(new :&: y) -> statefully (f y) new

instance Monad t => Monad (Stateful s t) where

instance Liftable (Stateful s) where
	lift x = Stateful $ \s -> ((:&:) s) <$> x

get :: Pointable t => Stateful s t s
get = Stateful $ point . delta

modify :: Pointable t => (s -> s) -> Stateful s t ()
modify f = Stateful $ \s -> point $ f s :&: ()

put :: Pointable t => s -> Stateful s t ()
put s = Stateful $ \_ -> point $ s :&: ()
