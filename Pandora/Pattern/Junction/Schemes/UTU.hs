module Pandora.Pattern.Junction.Schemes.UTU (UTU (..)) where

import Pandora.Core.Functor (Variant (Co), type (:.), type (>))
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

newtype UTU ct cu t u a = UTU (u :. t u > a)

instance Composition (UTU ct cu t u) where
	type Primary (UTU ct cu t u) a = u :. t u > a
	unwrap (UTU x) = x

instance (Covariant (t u), Covariant u) => Covariant (UTU 'Co 'Co t u) where
	f <$> UTU x = UTU $ f <$$> x

instance (Pointable (t u), Pointable u) => Pointable (UTU 'Co 'Co t u) where
	point = UTU . point . point

instance (Extractable (t u), Extractable u) => Extractable (UTU 'Co 'Co t u) where
	extract = extract . extract . unwrap

instance (Covariant (t u), Avoidable u) => Avoidable (UTU 'Co 'Co t u) where
	empty = UTU empty

instance (Covariant (t u), Alternative u) => Alternative (UTU 'Co 'Co t u) where
	UTU x <+> UTU y = UTU $ x <+> y

instance (Applicative (t u), Applicative u) => Applicative (UTU 'Co 'Co t u) where
	UTU f <*> UTU x = UTU $ apply <$> f <*> x

instance (Traversable (t u), Traversable u) => Traversable (UTU 'Co 'Co t u) where
	UTU x ->> f = UTU <$> x ->>> f

instance (Distributive (t u), Distributive u) => Distributive (UTU 'Co 'Co t u) where
	x >>- f = UTU . comap distribute . distribute $ unwrap . f <$> x

instance (forall u' . Pointable u', Liftable t) => Liftable (UTU 'Co 'Co t) where
	lift = UTU . point . lift

instance (forall u' . Extractable u', Lowerable t) => Lowerable (UTU 'Co 'Co t) where
	lower = lower . extract . unwrap

instance (forall u' . Setoid (u' :. t u' > a)) => Setoid (UTU 'Co 'Co t u a) where
	UTU x == UTU y = x == y

instance (forall u' . Chain (u' :. t u' > a)) => Chain (UTU 'Co 'Co t u a) where
	UTU x <=> UTU y = x <=> y

instance (forall u' . Semigroup (u' :. t u' > a)) => Semigroup (UTU 'Co 'Co t u a) where
	UTU x + UTU y = UTU $ x + y

instance (forall u' . Monoid (u' :. t u' > a)) => Monoid (UTU 'Co 'Co t u a) where
	zero = UTU zero
