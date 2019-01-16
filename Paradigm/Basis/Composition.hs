module Paradigm.Basis.Composition (T (..), TT (..), TTT (..)) where

import Core.Composition ((:.:))
import Core.Morphism ((.), ($))
import Core.Variant (Variant (Co, Contra))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Alternative (Alternative ((<+>)))
import Pattern.Functor.Applicative (Applicative ((<*>), apply))
import Pattern.Functor.Adjoint (Adjoint (phi, psi))
import Pattern.Functor.Contravariant (Contravariant ((>$<), contramap))

type Adjunctive t u = (Extractable t, Pointable t, Extractable u, Pointable u, Adjoint t u)


newtype T ct cu t u a = T { t :: (t :.: u) a }

instance (Covariant t, Covariant u) => Covariant (T Co Co t u) where
	f <$> T x = T $ (comap . comap) f x

instance (Covariant t, Contravariant u) => Contravariant (T Co Contra t u) where
	f >$< T x = T $ contramap f <$> x

instance (Contravariant t, Covariant u) => Contravariant (T Contra Co t u) where
	f >$< T x = T $ contramap (comap f) x

instance (Contravariant t, Contravariant u) => Covariant (T Contra Contra t u) where
	f <$> T x = T $ contramap (contramap f) x

instance (Applicative t, Applicative u) => Applicative (T Co Co t u) where
	T f <*> T x = T $ apply <$> f <*> x

instance (Alternative t, Covariant u) => Alternative (T Co Co t u) where
	T x <+> T y = T $ x <+> y

instance (Exclusive t, Covariant u) => Exclusive (T Co Co t u) where
	exclusive = T exclusive

instance (Pointable t, Pointable u) => Pointable (T Co Co t u) where
	point = T . point . point

instance (Extractable t, Extractable u) => Extractable (T Co Co t u) where
	extract = extract . extract . t

instance (Adjunctive t u, Adjunctive v w) => Adjoint (T Co Co t v) (T Co Co u w) where
	phi f = point . f . point
	psi f = extract . extract . comap f


newtype TT ct cu cv t u v a = TT { tt :: (t :.: u :.: v) a }

instance (Covariant t, Covariant u, Covariant v) => Covariant (TT Co Co Co t u v) where
	f <$> TT x = TT $ (comap . comap . comap) f x

instance (Covariant t, Covariant u, Contravariant v) => Contravariant (TT Co Co Contra t u v) where
	f >$< TT x = TT $ (comap . comap) (contramap f) x

instance (Covariant t, Contravariant u, Covariant v) => Contravariant (TT Co Contra Co t u v) where
	f >$< TT x = TT $ contramap (comap f) <$> x

instance (Contravariant t, Covariant u, Covariant v) => Contravariant (TT Contra Co Co t u v) where
	f >$< TT x = TT $ comap (comap f) >$< x

instance (Contravariant t, Contravariant u, Covariant v) => Covariant (TT Contra Contra Co t u v) where
	f <$> TT x = TT $ contramap (comap f) >$< x

instance (Covariant t, Contravariant u, Contravariant v) => Covariant (TT Co Contra Contra t u v) where
	f <$> TT x = TT $ contramap (contramap f) <$> x

instance (Contravariant t, Covariant u, Contravariant v) => Covariant (TT Contra Co Contra t u v) where
	f <$> TT x = TT $ comap (contramap f) >$< x

instance (Contravariant t, Contravariant u, Contravariant v) => Contravariant (TT Contra Contra Contra t u v) where
	f >$< TT x = TT $ (contramap . contramap . contramap) f x

instance (Applicative t, Applicative u, Applicative v) => Applicative (TT Co Co Co t u v) where
	TT f <*> TT x = TT $ (comap apply . (comap . comap) apply $ f) <*> x

instance (Alternative t, Covariant u, Covariant v) => Alternative (TT Co Co Co t u v) where
	TT x <+> TT y = TT $ x <+> y

instance (Exclusive t, Covariant u, Covariant v) => Exclusive (TT Co Co Co t u v) where
	exclusive = TT exclusive

instance (Pointable t, Pointable u, Pointable v) => Pointable (TT Co Co Co t u v) where
	point = TT . point . point . point

instance (Extractable t, Extractable u, Extractable v) => Extractable (TT Co Co Co t u v) where
	extract = extract . extract . extract . tt

instance (Adjunctive t w, Adjunctive v x, Adjunctive u y) => Adjoint (TT Co Co Co t v u) (TT Co Co Co w x y) where
	phi f = point . f . point
	psi f = extract . extract . comap f


newtype TTT ct cu cv cw t u v w a = TTT { ttt :: (t :.: u :.: v :.: w) a }

instance (Covariant t, Covariant u, Covariant v, Covariant w) => Covariant (TTT Co Co Co Co t u v w) where
	f <$> TTT x = TTT $ (comap . comap . comap . comap) f x

instance (Covariant t, Covariant u, Covariant v, Contravariant w) => Contravariant (TTT Co Co Co Contra t u v w) where
	f >$< TTT x = TTT $ (comap . comap . comap) (contramap f) x

instance (Covariant t, Covariant u, Contravariant v, Covariant w) => Contravariant (TTT Co Co Contra Co t u v w) where
	f >$< TTT x = TTT $ (comap . comap) (contramap (comap f)) x

instance (Covariant t, Contravariant u, Covariant v, Covariant w) => Contravariant (TTT Co Contra Co Co t u v w) where
	f >$< TTT x = TTT $ (contramap (comap (comap f))) <$> x

instance (Contravariant t, Covariant u, Covariant v, Covariant w) => Contravariant (TTT Contra Co Co Co t u v w) where
	f >$< TTT x = TTT $ comap (comap (comap f)) >$< x

instance (Contravariant t, Contravariant u, Covariant v, Covariant w) => Covariant (TTT Contra Contra Co Co t u v w) where
	f <$> TTT x = TTT $ (contramap . contramap . comap . comap $ f) x

instance (Covariant t, Contravariant u, Contravariant v, Covariant w) => Covariant (TTT Co Contra Contra Co t u v w) where
	f <$> TTT x = TTT $ (comap . contramap . contramap . comap $ f) x

instance (Covariant t, Covariant u, Contravariant v, Contravariant w) => Covariant (TTT Co Co Contra Contra t u v w) where
	f <$> TTT x = TTT $ (comap . comap) (contramap . contramap $ f) x

instance (Covariant t, Contravariant u, Covariant v, Contravariant w) => Covariant (TTT Co Contra Co Contra t u v w) where
	f <$> TTT x = TTT $ (comap . contramap . comap . contramap $ f) x

instance (Contravariant t, Covariant u, Contravariant v, Covariant w) => Covariant (TTT Contra Co Contra Co t u v w) where
	f <$> TTT x = TTT $ (contramap . comap . contramap . comap $ f) x

instance (Contravariant t, Covariant u, Covariant v, Contravariant w) => Covariant (TTT Contra Co Co Contra t u v w) where
	f <$> TTT x = TTT $ (contramap . comap . comap . contramap $ f) x

instance (Contravariant t, Contravariant u, Contravariant v, Covariant w) => Contravariant (TTT Contra Contra Contra Co t u v w) where
	f >$< TTT x = TTT $ (contramap . contramap . contramap . comap) f x

instance (Covariant t, Contravariant u, Contravariant v, Contravariant w) => Contravariant (TTT Co Contra Contra Contra t u v w) where
	f >$< TTT x = TTT $ (comap . contramap . contramap . contramap) f x

instance (Contravariant t, Covariant u, Contravariant v, Contravariant w) => Contravariant (TTT Contra Co Contra Contra t u v w) where
	f >$< TTT x = TTT $ (contramap . comap . contramap . contramap) f x

instance (Contravariant t, Contravariant u, Covariant v, Contravariant w) => Contravariant (TTT Contra Contra Co Contra t u v w) where
	f >$< TTT x = TTT $ (contramap . contramap . comap . contramap) f x

instance (Contravariant t, Contravariant u, Contravariant v, Contravariant w) => Covariant (TTT Contra Contra Contra Contra t u v w) where
	f <$> TTT x = TTT $ (contramap . contramap . contramap . contramap) f x

instance (Applicative t, Applicative u, Applicative v, Applicative w) => Applicative (TTT Co Co Co Co t u v w) where
	TTT f <*> TTT x = TTT $ (comap apply . (comap . comap) apply . (comap . comap . comap) apply $ f) <*> x

instance (Alternative t, Covariant u, Covariant v, Covariant w) => Alternative (TTT Co Co Co Co t u v w) where
	TTT x <+> TTT y = TTT $ x <+> y

instance (Exclusive t, Covariant u, Covariant v, Covariant w) => Exclusive (TTT Co Co Co Co t u v w) where
	exclusive = TTT exclusive

instance (Pointable t, Pointable u, Pointable v, Pointable w) => Pointable (TTT Co Co Co Co t u v w) where
	point = TTT . point . point . point . point

instance (Extractable t, Extractable u, Extractable v, Extractable w) => Extractable (TTT Co Co Co Co t u v w) where
	extract = extract . extract . extract . extract . ttt

instance (Adjunctive t u, Adjunctive t' u', Adjunctive t'' u'', Adjunctive t''' u''')
	=> Adjoint (TTT Co Co Co Co t t' t'' t''') (TTT Co Co Co Co u u' u'' u''') where
	phi f = point . f . point
	psi f = extract . extract . comap f
