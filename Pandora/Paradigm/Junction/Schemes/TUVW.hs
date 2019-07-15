module Pandora.Paradigm.Junction.Schemes.TUVW (TUVW (..)) where

import Pandora.Core.Functor (Variant (Co, Contra), type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Pattern.Junction.Composition (Composition (Primary, unwrap))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>), (<$$$>), (<$$$$>), comap))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<), (>$$<), (>$$$<), (>$$$$<), contramap))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>>>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-), distribute))
import Pandora.Pattern.Functor.Adjoint (Adjoint (phi, psi))

newtype TUVW ct cu cv cw t u v w a = TUVW (t :.: u :.: v :.: w >< a)

instance Composition (TUVW ct cu cv cw t u v w) where
	type Primary (TUVW ct cu cv cw t u v w) a = t :.: u :.: v :.: w >< a
	unwrap (TUVW x) = x

instance (Covariant t, Covariant u, Covariant v, Covariant w)
	=> Covariant (TUVW 'Co 'Co 'Co 'Co t u v w) where
	f <$> TUVW x = TUVW $ f <$$$$> x

instance (Covariant t, Covariant u, Covariant v, Contravariant w)
	=> Contravariant (TUVW 'Co 'Co 'Co 'Contra t u v w) where
	f >$< TUVW x = TUVW $ (f >$<) <$$$> x

instance (Covariant t, Covariant u, Contravariant v, Covariant w)
	=> Contravariant (TUVW 'Co 'Co 'Contra 'Co t u v w) where
	f >$< TUVW x = TUVW $ (contramap (comap f)) <$$> x

instance (Covariant t, Contravariant u, Covariant v, Covariant w)
	=> Contravariant (TUVW 'Co 'Contra 'Co 'Co t u v w) where
	f >$< TUVW x = TUVW $ (contramap (comap (comap f))) <$> x

instance (Contravariant t, Covariant u, Covariant v, Covariant w)
	=> Contravariant (TUVW 'Contra 'Co 'Co 'Co t u v w) where
	f >$< TUVW x = TUVW $ (f <$$$>) >$< x

instance (Contravariant t, Contravariant u, Covariant v, Covariant w)
	=> Covariant (TUVW 'Contra 'Contra 'Co 'Co t u v w) where
	f <$> TUVW x = TUVW $ (contramap . contramap . comap . comap $ f) x

instance (Covariant t, Contravariant u, Contravariant v, Covariant w)
	=> Covariant (TUVW 'Co 'Contra 'Contra 'Co t u v w) where
	f <$> TUVW x = TUVW $ (comap . contramap . contramap . comap $ f) x

instance (Covariant t, Covariant u, Contravariant v, Contravariant w)
	=> Covariant (TUVW 'Co 'Co 'Contra 'Contra t u v w) where
	f <$> TUVW x = TUVW $ (f >$$<) <$$> x

instance (Covariant t, Contravariant u, Covariant v, Contravariant w)
	=> Covariant (TUVW 'Co 'Contra 'Co 'Contra t u v w) where
	f <$> TUVW x = TUVW $ (comap . contramap . comap . contramap $ f) x

instance (Contravariant t, Covariant u, Contravariant v, Covariant w)
	=> Covariant (TUVW 'Contra 'Co 'Contra 'Co t u v w) where
	f <$> TUVW x = TUVW $ (contramap . comap . contramap . comap $ f) x

instance (Contravariant t, Covariant u, Covariant v, Contravariant w)
	=> Covariant (TUVW 'Contra 'Co 'Co 'Contra t u v w) where
	f <$> TUVW x = TUVW $ (contramap . comap . comap . contramap $ f) x

instance (Contravariant t, Contravariant u, Contravariant v, Covariant w)
	=> Contravariant (TUVW 'Contra 'Contra 'Contra 'Co t u v w) where
	f >$< TUVW x = TUVW $ (f <$>) >$$$< x

instance (Covariant t, Contravariant u, Contravariant v, Contravariant w)
	=> Contravariant (TUVW 'Co 'Contra 'Contra 'Contra t u v w) where
	f >$< TUVW x = TUVW $ (f >$$$<) <$> x

instance (Contravariant t, Covariant u, Contravariant v, Contravariant w)
	=> Contravariant (TUVW 'Contra 'Co 'Contra 'Contra t u v w) where
	f >$< TUVW x = TUVW $ (contramap . comap . contramap . contramap) f x

instance (Contravariant t, Contravariant u, Covariant v, Contravariant w)
	=> Contravariant (TUVW 'Contra 'Contra 'Co 'Contra t u v w) where
	f >$< TUVW x = TUVW $ (contramap . contramap . comap . contramap) f x

instance (Contravariant t, Contravariant u, Contravariant v, Contravariant w)
	=> Covariant (TUVW 'Contra 'Contra 'Contra 'Contra t u v w) where
	f <$> TUVW x = TUVW $ f >$$$$< x

instance (Pointable t, Pointable u, Pointable v, Pointable w)
	=> Pointable (TUVW 'Co 'Co 'Co 'Co t u v w) where
	point = TUVW . point . point . point . point

instance (Extractable t, Extractable u, Extractable v, Extractable w)
	=> Extractable (TUVW 'Co 'Co 'Co 'Co t u v w) where
	extract = extract . extract . extract . extract . unwrap

instance (Avoidable t, Covariant u, Covariant v, Covariant w)
	=> Avoidable (TUVW 'Co 'Co 'Co 'Co t u v w) where
	empty = TUVW empty

instance (Applicative t, Applicative u, Applicative v, Applicative w)
	=> Applicative (TUVW 'Co 'Co 'Co 'Co t u v w) where
	TUVW f <*> TUVW x = TUVW $ ((apply <$>) . (apply <$$>) . (apply <$$$>) $ f) <*> x

instance (Alternative t, Covariant u, Covariant v, Covariant w)
	=> Alternative (TUVW 'Co 'Co 'Co 'Co t u v w) where
	TUVW x <+> TUVW y = TUVW $ x <+> y

instance (Traversable t, Traversable u, Traversable v, Traversable w)
	=> Traversable (TUVW 'Co 'Co 'Co 'Co t u v w) where
	TUVW x ->> f = TUVW <$> x ->>>>> f

instance (Distributive t, Distributive u, Distributive v, Distributive w)
	=> Distributive (TUVW 'Co 'Co 'Co 'Co t u v w) where
	x >>- f = TUVW . (distribute <$$$>) . (distribute <$$>) . (distribute <$>) . distribute $ unwrap . f <$> x

type (:-|:) t u = (Extractable t, Pointable t, Extractable u, Pointable u, Adjoint t u)

instance (t :-|: u, v :-|: w, q :-|: q, r :-|: s)
	=> Adjoint (TUVW 'Co 'Co 'Co 'Co t v q r) (TUVW 'Co 'Co 'Co 'Co u w q s) where
	phi f = point . f . point
	psi f = extract . extract . comap f
