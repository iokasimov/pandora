module Pandora.Paradigm.Inventory.Stateful
	(Stateful (..), get, modify, put, fold, find) where

import Pandora.Core.Functor (Variant (Co), type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Junction.Composition (Composition (Outline, composition))
import Pandora.Paradigm.Junction.Transformer (Transformer (Layout, lay, equip))
import Pandora.Paradigm.Junction.Schemes.TUT (TUT (TUT))
import Pandora.Paradigm.Basis.Predicate (Predicate (predicate))
import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:), attached, delta, uncurry)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), ($>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Avoidable (Avoidable (idle))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Object.Setoid (bool)

newtype Stateful s a = Stateful ((->) s :.: (:*:) s >< a)

statefully :: s -> Stateful s a -> s :*: a
statefully initial (Stateful state) = state initial

instance Composition (Stateful s) where
	type Outline (Stateful s) a = (->) s :.: (:*:) s >< a
	composition (Stateful x) = x

instance Transformer (Stateful s) where
	type Layout (Stateful s) u a = TUT 'Co 'Co (Stateful s) u a
	lay x = TUT $ \s -> (s :*:) <$> x
	equip x = TUT $ point <$> composition x

instance Covariant (Stateful s) where
	f <$> Stateful x = Stateful $ \old -> f <$> x old

instance Applicative (Stateful s) where
	Stateful f <*> Stateful x = Stateful $ \old ->
		let latest = attached . x $ old in
			latest :*: (extract (f old) . extract . x $ old)

instance Pointable (Stateful s) where
	point x = Stateful $ \s -> s :*: x

instance Bindable (Stateful s) where
	Stateful x >>= f = Stateful $ \old ->
		uncurry statefully $ f <$> x old

instance Monad (Stateful s) where

get :: Stateful s s
get = Stateful delta

modify :: (s -> s) -> Stateful s ()
modify f = Stateful $ \s -> f s :*: ()

put :: s -> Stateful s ()
put s = Stateful $ \_ -> s :*: ()

fold :: Traversable t => s -> (a -> s -> s) -> t a -> s
fold start op struct = extract . statefully start $
	struct ->> modify . op $> () *> get

find :: (Pointable u, Avoidable u, Alternative u, Traversable t) => Predicate a -> t a -> u a
find p struct = fold idle (\x s -> (<+>) s . bool idle (point x) . predicate p $ x) struct
