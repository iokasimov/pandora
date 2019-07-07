module Pandora.Paradigm.Junction.Transformer (UT (..), type (:!:), up, Y (..), type (:>:)) where

import Pandora.Core.Functor (type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>), comap))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Avoidable (Avoidable (idle))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-), distribute))
import Pandora.Pattern.Functor.Liftable (Liftable (lift))
import Pandora.Pattern.Functor.Lowerable (Lowerable (lower))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))

infixr 0 :!:, :>:
type (:!:) t u = UT t u
type (:>:) t u = Y t u

newtype UT t u a = UT { ut :: u :.: t >< a }

instance (Covariant t, Covariant u) => Covariant (UT t u) where
	f <$> UT x = UT $ f <$$> x

instance (Pointable t, Pointable u) => Pointable (UT t u) where
	point = UT . point . point

instance (Extractable t, Extractable u) => Extractable (UT t u) where
	extract = extract . extract . ut

instance (Covariant t, Avoidable u) => Avoidable (UT t u) where
	idle = UT idle

instance (Covariant t, Alternative u) => Alternative (UT t u) where
	UT x <+> UT y = UT $ x <+> y

instance (Applicative t, Applicative u) => Applicative (UT t u) where
	UT f <*> UT x = UT $ apply <$> f <*> x

instance Pointable t => Liftable (UT t) where
	lift x = UT $ point <$> x

instance Extractable t => Lowerable (UT t) where
	lower (UT x) = extract <$> x

instance (Traversable t, Traversable u) => Traversable (UT t u) where
	UT x ->> f = UT <$> x ->>> f

instance (Distributive t, Distributive u) => Distributive (UT t u) where
	x >>- f = UT . comap distribute . distribute $ ut . f <$> x

instance Setoid (u :.: t >< a) => Setoid (UT t u a) where
	UT x == UT y = x == y

instance Chain (u :.: t >< a) => Chain (UT t u a) where
	UT x <=> UT y = x <=> y

instance Semigroup (u :.: t >< a) => Semigroup (UT t u a) where
	UT x + UT y = UT $ x + y

instance Monoid (u :.: t >< a) => Monoid (UT t u a) where
	zero = UT zero

up :: Pointable u => t a -> UT t u a
up = UT . point


newtype Y t u a = Y { y :: u :.: t u >< a }

instance (Covariant (t u), Covariant u) => Covariant (Y t u) where
	f <$> Y x = Y $ f <$$> x

instance (Pointable (t u), Pointable u) => Pointable (Y t u) where
	point = Y . point . point

instance (Extractable (t u), Extractable u) => Extractable (Y t u) where
	extract = extract . extract . y

instance (Covariant (t u), Avoidable u) => Avoidable (Y t u) where
	idle = Y idle

instance (Covariant (t u), Alternative u) => Alternative (Y t u) where
	Y x <+> Y y = Y $ x <+> y

instance (Applicative (t u), Applicative u) => Applicative (Y t u) where
	Y f <*> Y x = Y $ apply <$> f <*> x

instance (Traversable (t u), Traversable u) => Traversable (Y t u) where
	Y x ->> f = Y <$> x ->>> f

instance (Distributive (t u), Distributive u) => Distributive (Y t u) where
	x >>- f = Y . comap distribute . distribute $ y . f <$> x

instance (forall u' . Pointable u', Liftable t) => Liftable (Y t) where
	lift = Y . point . lift

instance (forall u' . Extractable u', Lowerable t) => Lowerable (Y t) where
	lower = lower . extract . y

instance (forall u' . Setoid (u' :.: t u' >< a)) => Setoid (Y t u a) where
	Y x == Y y = x == y

instance (forall u' . Chain (u' :.: t u' >< a)) => Chain (Y t u a) where
	Y x <=> Y y = x <=> y

instance (forall u' . Semigroup (u' :.: t u' >< a)) => Semigroup (Y t u a) where
	Y x + Y y = Y $ x + y

instance (forall u' . Monoid (u' :.: t u' >< a)) => Monoid (Y t u a) where
	zero = Y zero
