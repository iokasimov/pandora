module Paradigm.Basis.Functor.Transformer (T (..), type (:!:), up) where

import Core.Composition ((:.:))
import Core.Morphism ((.), ($))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pattern.Functor.Alternative (Alternative ((<+>)))
import Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pattern.Functor.Traversable (Traversable ((->>), traverse))
import Pattern.Functor.Distributive (Distributive ((>>-), distribute))
import Pattern.Functor.Bindable (Bindable ((>>=), bind))
import Pattern.Functor.Liftable (Liftable (lift))
import Pattern.Functor.Lowerable (Lowerable (lower))

newtype T t u a = T { t :: (u :.: t) a }

infixr 0 :!:
type (:!:) t u = T t u

instance (Covariant t, Covariant u) => Covariant (T t u) where
	f <$> T x = T $ (comap . comap) f x

instance (Pointable t, Pointable u) => Pointable (T t u) where
	point = T . point . point

instance (Extractable t, Extractable u) => Extractable (T t u) where
	extract = extract . extract . t

instance (Covariant t, Exclusive u) => Exclusive (T t u) where
	exclusive = T exclusive

instance (Covariant t, Alternative u) => Alternative (T t u) where
	T x <+> T y = T $ x <+> y

instance (Applicative t, Applicative u) => Applicative (T t u) where
	T f <*> T x = T $ apply <$> f <*> x

instance Pointable t => Liftable (T t) where
	lift x = T $ point <$> x

instance Extractable t => Lowerable (T t) where
	lower (T x) = extract <$> x

instance (Traversable t, Traversable u) => Traversable (T t u) where
	T x ->> f = T <$> (traverse . traverse) f x

instance (Distributive t, Distributive u) => Distributive (T t u) where
	x >>- f = T . comap distribute . distribute $ t . f <$> x

up :: Pointable u => t a -> T t u a
up = T . point
