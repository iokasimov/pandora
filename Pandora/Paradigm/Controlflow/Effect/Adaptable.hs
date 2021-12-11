{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Effect.Adaptable where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Transformer (Liftable (lift), Lowerable (lower))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))
import Pandora.Paradigm.Primary.Algebraic (Extractable, extract, point)
import Pandora.Paradigm.Primary.Functor.Identity (Identity)
import Pandora.Paradigm.Controlflow.Effect.Transformer (Monadic, Comonadic, wrap, bring, (:>), (:<))

class Adaptable m t u where
	{-# MINIMAL adapt #-}
	adapt :: m (t a) (u a)

instance Category m => Adaptable m t t where
	adapt = identity @m

instance {-# OVERLAPS #-} Monoidal (-->) (-->) (:*:) (:*:) u => Adaptable (->) Identity u where
	adapt = point . extract

instance (Monadic m t, Monoidal (-->) (-->) (:*:) (:*:) u) => Adaptable m t (t :> u) where
	adapt = wrap

instance (Covariant m m u, Monadic m t, Liftable m ((:>) t)) => Adaptable m u (t :> u) where
	adapt = lift

instance (Covariant m m u', Liftable m ((:>) t), Adaptable m u u') => Adaptable m u (t :> u') where
	adapt = lift . adapt

instance (Comonadic m t, Extractable u) => Adaptable m (t :< u) t where
	adapt = bring

instance (Covariant m m u', Lowerable m ((:<) t), Adaptable m u' u) => Adaptable m (t :< u') u where
	adapt = adapt . lower

--instance (Covariant m m u, Hoistable ((:>) t), Adaptable m u u') => Adaptable m (t :> u) (t :> u') where
--	adapt = (adapt /|\)
--
--instance
--	( Covariant m m v
--	, Covariant m m (Schematic Monad u v)
--	, Hoistable ((:>) (t :> u))
--	, Hoistable (Schematic Monad t)
--	, Hoistable (Schematic Monad u)
--	, Adaptable m v v'
--	) => Adaptable m (t :> u :> v) (t :> u :> v') where
--	adapt = ((adapt /|\) /|\)
--
--instance
--	( Covariant m m u
--	, Covariant m m v
--	, Covariant m m w
--	, Covariant m m (Schematic Monad u v)
--	, Covariant m m (Schematic Monad u (v :> w))
--	, Covariant m m (Schematic Monad v w)
--	, Hoistable ((:>) (t :> u :> v))
--	, Hoistable (Schematic Monad t)
--	, Hoistable (Schematic Monad u)
--	, Hoistable (Schematic Monad v)
--	, Adaptable m w w'
--	) => Adaptable m (t :> u :> v :> w) (t :> u :> v :> w') where
--	adapt = (((adapt /|\) /|\) /|\)
--
--instance
--	( Covariant m m x
--	, Covariant m m (Schematic Monad u (v :> (w :> x)))
--	, Covariant m m (Schematic Monad v (w :> x))
--	, Covariant m m (Schematic Monad w x)
--	, Hoistable ((:>) (t :> u :> v))
--	, Hoistable (Schematic Monad t)
--	, Hoistable (Schematic Monad u)
--	, Hoistable (Schematic Monad v)
--	, Hoistable (Schematic Monad w)
--	, Adaptable m x x'
--	) => Adaptable m (t :> u :> v :> w :> x) (t :> u :> v :> w :> x') where
--	adapt = (((adapt /|\) /|\) /|\)
--
--instance
--	( Covariant m m y
--	, Covariant m m (Schematic Monad u (v :> (w :> (x :> y))))
--	, Covariant m m (Schematic Monad v (w :> (x :> y)))
--	, Covariant m m (Schematic Monad w (x :> y))
--	, Covariant m m (Schematic Monad x y)
--	, Hoistable ((:>) (t :> u :> v :> w))
--	, Hoistable (Schematic Monad t)
--	, Hoistable (Schematic Monad u)
--	, Hoistable (Schematic Monad v)
--	, Hoistable (Schematic Monad w)
--	, Hoistable (Schematic Monad x)
--	, Adaptable m y y'
--	) => Adaptable m (t :> u :> v :> w :> x :> y) (t :> u :> v :> w :> x :> y') where
--	adapt = ((((adapt /|\) /|\) /|\) /|\)
--
--instance
--	( Covariant m m z
--	, Covariant m m (Schematic Monad u (v :> (w :> (x :> (y :> z)))))
--	, Covariant m m (Schematic Monad v (w :> (x :> (y :> z))))
--	, Covariant m m (Schematic Monad w (x :> (y :> z)))
--	, Covariant m m (Schematic Monad x (y :> z))
--	, Covariant m m (Schematic Monad y z)
--	, Hoistable ((:>) (t :> u :> v :> w))
--	, Hoistable (Schematic Monad t)
--	, Hoistable (Schematic Monad u)
--	, Hoistable (Schematic Monad v)
--	, Hoistable (Schematic Monad w)
--	, Hoistable (Schematic Monad x)
--	, Hoistable (Schematic Monad y)
--	, Adaptable m z z'
--	) => Adaptable m (t :> u :> v :> w :> x :> y :> z)
--		(t :> u :> v :> w :> x :> y :> z') where
--	adapt = (((((adapt /|\) /|\) /|\) /|\) /|\)
--
--instance
--	( Covariant m m f
--	, Covariant m m (Schematic Monad u (v :> (w :> (x :> (y :> (z :> f))))))
--	, Covariant m m (Schematic Monad v (w :> (x :> (y :> (z :> f)))))
--	, Covariant m m (Schematic Monad w (x :> (y :> (z :> f))))
--	, Covariant m m (Schematic Monad x (y :> (z :> f)))
--	, Covariant m m (Schematic Monad y (z :> f))
--	, Covariant m m (Schematic Monad z f)
--	, Hoistable ((:>) (t :> u :> v :> w))
--	, Hoistable (Schematic Monad t)
--	, Hoistable (Schematic Monad u)
--	, Hoistable (Schematic Monad v)
--	, Hoistable (Schematic Monad w)
--	, Hoistable (Schematic Monad x)
--	, Hoistable (Schematic Monad y)
--	, Hoistable (Schematic Monad z)
--	, Adaptable m f f'
--	) => Adaptable m (t :> u :> v :> w :> x :> y :> z :> f)
--		(t :> u :> v :> w :> x :> y :> z :> f') where
--	adapt = ((((((adapt /|\) /|\) /|\) /|\) /|\) /|\)
--
--instance
--	( Covariant m m h
--	, Covariant m m (Schematic Monad u (v :> (w :> (x :> (y :> (z :> (f :> h)))))))
--	, Covariant m m (Schematic Monad v (w :> (x :> (y :> (z :> (f :> h))))))
--	, Covariant m m (Schematic Monad w (x :> (y :> (z :> (f :> h)))))
--	, Covariant m m (Schematic Monad x (y :> (z :> (f :> h))))
--	, Covariant m m (Schematic Monad y (z :> (f :> h)))
--	, Covariant m m (Schematic Monad z (f :> h))
--	, Covariant m m (Schematic Monad f h)
--	, Hoistable ((:>) (t :> u :> v :> w))
--	, Hoistable (Schematic Monad t)
--	, Hoistable (Schematic Monad u)
--	, Hoistable (Schematic Monad v)
--	, Hoistable (Schematic Monad w)
--	, Hoistable (Schematic Monad x)
--	, Hoistable (Schematic Monad y)
--	, Hoistable (Schematic Monad z)
--	, Hoistable (Schematic Monad f)
--	, Adaptable m h h'
--	) => Adaptable m (t :> u :> v :> w :> x :> y :> z :> f :> h)
--		(t :> u :> v :> w :> x :> y :> z :> f :> h') where
--	adapt = (((((((adapt /|\) /|\) /|\) /|\) /|\) /|\) /|\)
