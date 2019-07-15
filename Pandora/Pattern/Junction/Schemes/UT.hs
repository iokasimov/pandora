module Pandora.Pattern.Junction.Schemes.UT (UT (..)) where

import Pandora.Core.Functor (Variant (Co), type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Pattern.Junction.Composition (Composition (Primary, unwrap))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>), comap))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
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

newtype UT ct cu t u a = UT (u :.: t >< a)

instance Composition (UT ct cu t u) where
	type Primary (UT ct cu t u) a = u :.: t >< a
	unwrap (UT x) = x

instance (Covariant t, Covariant u) => Covariant (UT 'Co 'Co t u) where
	f <$> UT x = UT $ f <$$> x

instance (Pointable t, Pointable u) => Pointable (UT 'Co 'Co t u) where
	point = UT . point . point

instance (Extractable t, Extractable u) => Extractable (UT 'Co 'Co t u) where
	extract = extract . extract . unwrap

instance (Covariant t, Avoidable u) => Avoidable (UT 'Co 'Co t u) where
	empty = UT empty

instance (Covariant t, Alternative u) => Alternative (UT 'Co 'Co t u) where
	UT x <+> UT y = UT $ x <+> y

instance (Applicative t, Applicative u) => Applicative (UT 'Co 'Co t u) where
	UT f <*> UT x = UT $ apply <$> f <*> x

instance Pointable t => Liftable (UT 'Co 'Co t) where
	lift x = UT $ point <$> x

instance Extractable t => Lowerable (UT 'Co 'Co t) where
	lower (UT x) = extract <$> x

instance (Traversable t, Traversable u) => Traversable (UT 'Co 'Co t u) where
	UT x ->> f = UT <$> x ->>> f

instance (Distributive t, Distributive u) => Distributive (UT 'Co 'Co t u) where
	x >>- f = UT . comap distribute . distribute $ unwrap . f <$> x

instance Setoid (u :.: t >< a) => Setoid (UT 'Co 'Co t u a) where
	UT x == UT y = x == y

instance Chain (u :.: t >< a) => Chain (UT 'Co 'Co t u a) where
	UT x <=> UT y = x <=> y

instance Semigroup (u :.: t >< a) => Semigroup (UT 'Co 'Co t u a) where
	UT x + UT y = UT $ x + y

instance Monoid (u :.: t >< a) => Monoid (UT 'Co 'Co t u a) where
	zero = UT zero
