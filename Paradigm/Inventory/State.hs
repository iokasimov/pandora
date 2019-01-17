module Paradigm.Inventory.State (Stateful (..), State) where

import Core.Composition ((:.:))
import Core.Morphism ((.), ($))
import Paradigm.Basis.Identity (Identity)
import Paradigm.Basis.Product (Product ((:&:)), type (:&:))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Applicative (Applicative ((<*>)))
import Pattern.Functor.Bindable (Bindable ((>>=)))
import Pattern.Functor.Monad (Monad)
import Pattern.Functor.Liftable (Liftable (lift))

newtype Stateful s t a = Stateful { stateful :: ((->) s :.: t :.: (:&:) s) a }

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
		x old >>= \(new :&: y) -> stateful (f y) new

instance Monad t => Monad (Stateful s t) where

instance Liftable (Stateful s) where
	lift x = Stateful $ \s -> ((:&:) s) <$> x
