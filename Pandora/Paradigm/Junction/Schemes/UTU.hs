module Pandora.Paradigm.Junction.Schemes.UTU (UTU (..), type (:>:)) where

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

infixr 0 :>:
type (:>:) t u = UTU t u

newtype UTU t u a = UTU { utu :: u :.: t u >< a }

instance (Covariant (t u), Covariant u) => Covariant (UTU t u) where
	f <$> UTU x = UTU $ f <$$> x

instance (Pointable (t u), Pointable u) => Pointable (UTU t u) where
	point = UTU . point . point

instance (Extractable (t u), Extractable u) => Extractable (UTU t u) where
	extract = extract . extract . utu

instance (Covariant (t u), Avoidable u) => Avoidable (UTU t u) where
	idle = UTU idle

instance (Covariant (t u), Alternative u) => Alternative (UTU t u) where
	UTU x <+> UTU utu = UTU $ x <+> utu

instance (Applicative (t u), Applicative u) => Applicative (UTU t u) where
	UTU f <*> UTU x = UTU $ apply <$> f <*> x

instance (Traversable (t u), Traversable u) => Traversable (UTU t u) where
	UTU x ->> f = UTU <$> x ->>> f

instance (Distributive (t u), Distributive u) => Distributive (UTU t u) where
	x >>- f = UTU . comap distribute . distribute $ utu . f <$> x

instance (forall u' . Pointable u', Liftable t) => Liftable (UTU t) where
	lift = UTU . point . lift

instance (forall u' . Extractable u', Lowerable t) => Lowerable (UTU t) where
	lower = lower . extract . utu

instance (forall u' . Setoid (u' :.: t u' >< a)) => Setoid (UTU t u a) where
	UTU x == UTU utu = x == utu

instance (forall u' . Chain (u' :.: t u' >< a)) => Chain (UTU t u a) where
	UTU x <=> UTU utu = x <=> utu

instance (forall u' . Semigroup (u' :.: t u' >< a)) => Semigroup (UTU t u a) where
	UTU x + UTU utu = UTU $ x + utu

instance (forall u' . Monoid (u' :.: t u' >< a)) => Monoid (UTU t u a) where
	zero = UTU zero
