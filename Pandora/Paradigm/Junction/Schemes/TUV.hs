module Pandora.Paradigm.Junction.Schemes.TUV (TUV (..)) where

import Pandora.Core.Functor (Variant (Co, Contra), type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Pattern.Junction.Composition (Composition (Primary, unwrap))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>), (<$$$>), comap))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<), (>$$<), (>$$$<)), contramap)
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-), distribute))
import Pandora.Pattern.Functor.Adjoint (Adjoint (phi, psi))

newtype TUV ct cu cv t u v a = TUV (t :.: u :.: v >< a)

instance Composition (TUV ct cu cv t u v) where
	type Primary (TUV ct cu cv t u v) a = t :.: u :.: v >< a
	unwrap (TUV x) = x

instance (Covariant t, Covariant u, Covariant v)
	=> Covariant (TUV 'Co 'Co 'Co t u v) where
	f <$> TUV x = TUV $ f <$$$> x

instance (Covariant t, Covariant u, Contravariant v)
	=> Contravariant (TUV 'Co 'Co 'Contra t u v) where
	f >$< TUV x = TUV $ (f >$<) <$$> x

instance (Covariant t, Contravariant u, Covariant v)
	=> Contravariant (TUV 'Co 'Contra 'Co t u v) where
	f >$< TUV x = TUV $ contramap (comap f) <$> x

instance (Contravariant t, Covariant u, Covariant v)
	=> Contravariant (TUV 'Contra 'Co 'Co t u v) where
	f >$< TUV x = TUV $ (f <$$>) >$< x

instance (Contravariant t, Contravariant u, Covariant v)
	=> Covariant (TUV 'Contra 'Contra 'Co t u v) where
	f <$> TUV x = TUV $ contramap (comap f) >$< x

instance (Covariant t, Contravariant u, Contravariant v)
	=> Covariant (TUV 'Co 'Contra 'Contra t u v) where
	f <$> TUV x = TUV $ (f >$$<) <$> x

instance (Contravariant t, Covariant u, Contravariant v)
	=> Covariant (TUV 'Contra 'Co 'Contra t u v) where
	f <$> TUV x = TUV $ comap (contramap f) >$< x

instance (Contravariant t, Contravariant u, Contravariant v)
	=> Contravariant (TUV 'Contra 'Contra 'Contra t u v) where
	f >$< TUV x = TUV $ f >$$$< x

instance (Pointable t, Pointable u, Pointable v)
	=> Pointable (TUV 'Co 'Co 'Co t u v) where
	point = TUV . point . point . point

instance (Extractable t, Extractable u, Extractable v)
	=> Extractable (TUV 'Co 'Co 'Co t u v) where
	extract = extract . extract . extract . unwrap

instance (Avoidable t, Covariant u, Covariant v)
	=> Avoidable (TUV 'Co 'Co 'Co t u v) where
	empty = TUV empty

instance (Applicative t, Applicative u, Applicative v)
	=> Applicative (TUV 'Co 'Co 'Co t u v) where
	TUV f <*> TUV x = TUV $ ((apply <$>) . (apply <$$>) $ f) <*> x

instance (Alternative t, Covariant u, Covariant v)
	=> Alternative (TUV 'Co 'Co 'Co t u v) where
	TUV x <+> TUV y = TUV $ x <+> y

instance (Traversable t, Traversable u, Traversable v)
	=> Traversable (TUV 'Co 'Co 'Co t u v) where
	TUV x ->> f = TUV <$> x ->>>> f

instance (Distributive t, Distributive u, Distributive v)
	=> Distributive (TUV 'Co 'Co 'Co t u v) where
	x >>- f = TUV . (distribute <$$>) . (distribute <$>) . distribute $ unwrap . f <$> x

type (:-|:) t u = (Extractable t, Pointable t, Extractable u, Pointable u, Adjoint t u)

instance (t :-|: w, v :-|: x, u :-|: y)
	=> Adjoint (TUV 'Co 'Co 'Co t v u) (TUV 'Co 'Co 'Co w x y) where
	phi f = point . f . point
	psi f = extract . extract . comap f
