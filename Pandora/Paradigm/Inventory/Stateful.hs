module Pandora.Paradigm.Inventory.Stateful (Stateful (..), State, get, modify, put, fold, find) where

import Pandora.Core.Functor (type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Identity (Identity)
import Pandora.Paradigm.Basis.Predicate (Predicate (predicate))
import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:), delta)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), ($>), (<$$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Avoidable (Avoidable (idle))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Liftable (Liftable (lift))
import Pandora.Pattern.Object.Setoid (bool)

newtype Stateful s t a = Stateful ((->) s :.: t :.: (:*:) s >< a)

statefully :: s -> Stateful s t a -> t (s :*: a)
statefully initial (Stateful state) = state initial

type State s = Stateful s Identity

instance Covariant t => Covariant (Stateful s t) where
	f <$> Stateful x = Stateful $ \old -> f <$$> x old

instance Bindable t => Applicative (Stateful s t) where
	Stateful f <*> Stateful x = Stateful $ \old ->
		f old >>= \(new :*: g) -> g <$$> x new

instance Pointable t => Pointable (Stateful s t) where
	point x = Stateful $ \s -> point $ s :*: x

instance Bindable t => Bindable (Stateful s t) where
	Stateful x >>= f = Stateful $ \old ->
		x old >>= \(new :*: y) -> statefully new (f y)

instance Monad t => Monad (Stateful s t) where

instance Liftable (Stateful s) where
	lift x = Stateful $ \s -> ((:*:) s) <$> x

get :: Pointable t => Stateful s t s
get = Stateful $ point . delta

modify :: Pointable t => (s -> s) -> Stateful s t ()
modify f = Stateful $ \s -> point $ f s :*: ()

put :: Pointable t => s -> Stateful s t ()
put s = Stateful $ \_ -> point $ s :*: ()

fold :: Traversable t => s -> (a -> s -> s) -> t a -> s
fold start op struct = extract . extract @Identity $
	statefully start (struct ->> (modify . op) $> () *> get)

find :: (Pointable u, Avoidable u, Alternative u, Traversable t) => Predicate a -> t a -> u a
find p struct = fold idle (\x s -> (<+>) s . bool idle (point x) . predicate p $ x) struct
