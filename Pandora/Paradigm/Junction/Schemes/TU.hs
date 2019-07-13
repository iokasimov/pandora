module Pandora.Paradigm.Junction.Schemes.TU (TU (..)) where

import Pandora.Core.Functor (Variant (Co, Contra), type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Junction.Composition (Composition (Outline, composition))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>), comap))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<), (>$$<)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-), distribute))
import Pandora.Pattern.Functor.Adjoint (Adjoint (phi, psi))

newtype TU ct cu t u a = TU (t :.: u >< a)

instance Composition (TU ct cu t u) where
	type Outline (TU ct cu t u) a = t :.: u >< a
	composition (TU x) = x

instance (Covariant t, Covariant u) => Covariant (TU 'Co 'Co t u) where
	f <$> TU x = TU $ f <$$> x

instance (Covariant t, Contravariant u) => Contravariant (TU 'Co 'Contra t u) where
	f >$< TU x = TU $ (f >$<) <$> x

instance (Contravariant t, Covariant u) => Contravariant (TU 'Contra 'Co t u) where
	f >$< TU x = TU $ (f <$>) >$< x

instance (Contravariant t, Contravariant u) => Covariant (TU 'Contra 'Contra t u) where
	f <$> TU x = TU $ f >$$< x

instance (Pointable t, Pointable u) => Pointable (TU 'Co 'Co t u) where
	point = TU . point . point

instance (Extractable t, Extractable u) => Extractable (TU 'Co 'Co t u) where
	extract = extract . extract . composition

instance (Avoidable t, Covariant u) => Avoidable (TU 'Co 'Co t u) where
	empty = TU empty

instance (Applicative t, Applicative u) => Applicative (TU 'Co 'Co t u) where
	TU f <*> TU x = TU $ apply <$> f <*> x

instance (Alternative t, Covariant u) => Alternative (TU 'Co 'Co t u) where
	TU x <+> TU y = TU $ x <+> y

instance (Traversable t, Traversable u) => Traversable (TU 'Co 'Co t u) where
	TU x ->> f = TU <$> x ->>> f

instance (Distributive t, Distributive u) => Distributive (TU 'Co 'Co t u) where
	x >>- f = TU . comap distribute . distribute $ composition . f <$> x

type (:-|:) t u = (Extractable t, Pointable t, Extractable u, Pointable u, Adjoint t u)

instance (t :-|: u, v :-|: w) => Adjoint (TU 'Co 'Co t v) (TU 'Co 'Co u w) where
	phi f = point . f . point
	psi f = extract . extract . comap f
