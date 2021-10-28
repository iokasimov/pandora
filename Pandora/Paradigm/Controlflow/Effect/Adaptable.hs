{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Effect.Adaptable where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (Category (identity))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer (Liftable (lift), Lowerable (lower), Hoistable ((/|\)))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))
import Pandora.Paradigm.Primary.Algebraic (Extractable)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic)
import Pandora.Paradigm.Controlflow.Effect.Transformer (Monadic, Comonadic, wrap, bring, (:>), (:<))

class Adaptable m t u where
	{-# MINIMAL adapt #-}
	adapt :: m (t a) (u a)

type Lifting m t u = (Monadic m t, Liftable m (Schematic Monad t), Covariant m m u)
type Lowering m t u = (Comonadic m t, Lowerable m (Schematic Comonad t), Covariant m m u)
type Wrappable m t u = (Monadic m t, Monoidal (-->) (-->) (:*:) (:*:) u)
type Bringable m t u = (Comonadic m t, Extractable u)

instance Category m => Adaptable m t t where
	adapt = identity @m

instance (Covariant m m u, Liftable m ((:>) t)) => Adaptable m u (t :> u) where
	adapt = lift

instance Wrappable m t u => Adaptable m t (t :> u) where
	adapt = wrap

instance (Covariant m m u, Lowerable m ((:<) t)) => Adaptable m (t :< u) u where
	adapt = lower

instance Bringable m t u => Adaptable m (t :< u) t where
	adapt = bring

instance
	( Wrappable m u v
	, Covariant m m (u :> v)
	, Liftable m ((:>) t)
	) => Adaptable m u (t :> u :> v) where
	adapt = lift . wrap

instance
	( Covariant m m v
	, Covariant m m (u :> v)
	, Liftable m ((:>) t)
	, Liftable m ((:>) u)
	) => Adaptable m v (t :> u :> v) where
	adapt = lift . lift

instance
	( Lowering m t (Schematic Comonad u v)
	, Bringable m u v
	, Covariant m m (u :< v)
	, Lowerable m ((:<) t)
	) => Adaptable m (t :< u :< v) u where
	adapt = bring . lower

instance
	( Covariant m m v
	, Covariant m m (u :< v)
	, Lowerable m ((:<) t)
	, Lowerable m ((:<) u)
	) => Adaptable m (t :< u :< v) v where
	adapt = lower . lower

--instance
--	( Liftable m (Schematic Monad t)
--	, Lifting m t (Schematic Monad u (v :> w))
--	, Lifting m u (Schematic Monad v w)
--	, Wrappable m v w
--	) => Adaptable m v (t :> u :> v :> w) where
--	adapt = lift . lift . wrap
--
--instance
--	( Lifting m t (Schematic Monad u v)
--	, Lifting m t (Schematic Monad u (v :> w))
--	, Lifting m u (Schematic Monad v w)
--	, Lifting m v w
--	) => Adaptable m w (t :> u :> v :> w) where
--	adapt = lift . lift . lift
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w))
--	, Lowering m u (Schematic Comonad v w)
--	, Bringable m v w
--	) => Adaptable m (t :< u :< v :< w) v where
--	adapt = bring . lower . lower
--
--instance
--	( Lowering m t (Schematic Comonad u v)
--	, Lowering m t (Schematic Comonad u (v :< w))
--	, Lowering m u (Schematic Comonad v w)
--	, Lowering m v w
--	) => Adaptable m (t :< u :< v :< w) w where
--	adapt = lower . lower . lower
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x))
--	, Lifting m u (Schematic Monad v (w :> x))
--	, Lifting m v (Schematic Monad w x)
--	, Lifting m w x
--	) => Adaptable m x (t :> u :> v :> w :> x) where
--	adapt = lift . lift . lift . lift
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x))
--	, Lifting m u (Schematic Monad v (w :> x))
--	, Lifting m v (Schematic Monad w x)
--	, Wrappable m w x
--	) => Adaptable m w (t :> u :> v :> w :> x) where
--	adapt = lift . lift . lift . wrap
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x))
--	, Lowering m u (Schematic Comonad v (w :< x))
--	, Lowering m v (Schematic Comonad w x)
--	, Lowering m w x
--	) => Adaptable m (t :< u :< v :< w :< x) x where
--	adapt = lower . lower . lower . lower
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x))
--	, Lowering m u (Schematic Comonad v (w :< x))
--	, Lowering m v (Schematic Comonad w x)
--	, Bringable m w x
--	) => Adaptable m (t :< u :< v :< w :< x) w where
--	adapt = bring . lower . lower . lower
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x :> y))
--	, Lifting m u (Schematic Monad v (w :> x :> y))
--	, Lifting m v (Schematic Monad w (x :> y))
--	, Lifting m w (Schematic Monad x y)
--	, Lifting m x y
--	) => Adaptable m y (t :> u :> v :> w :> x :> y) where
--	adapt = lift . lift . lift . lift . lift
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x :> y))
--	, Lifting m u (Schematic Monad v (w :> x :> y))
--	, Lifting m v (Schematic Monad w (x :> y))
--	, Lifting m w (Schematic Monad x y)
--	, Wrappable m x y
--	) => Adaptable m x (t :> u :> v :> w :> x :> y) where
--	adapt = lift . lift . lift . lift . wrap
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x :< y))
--	, Lowering m u (Schematic Comonad v (w :< x :< y))
--	, Lowering m v (Schematic Comonad w (x :< y))
--	, Lowering m w (Schematic Comonad x y)
--	, Lowering m x y
--	) => Adaptable m (t :< u :< v :< w :< x :< y) y where
--	adapt = lower . lower . lower . lower . lower
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x :< y))
--	, Lowering m u (Schematic Comonad v (w :< x :< y))
--	, Lowering m v (Schematic Comonad w (x :< y))
--	, Lowering m w (Schematic Comonad x y)
--	, Bringable m x y
--	) => Adaptable m (t :< u :< v :< w :< x :< y) x where
--	adapt = bring . lower . lower . lower . lower
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x :> y :> z))
--	, Lifting m u (Schematic Monad v (w :> x :> y :> z))
--	, Lifting m v (Schematic Monad w (x :> y :> z))
--	, Lifting m w (Schematic Monad x (y :> z))
--	, Lifting m x (Schematic Monad y z)
--	, Lifting m y z
--	) => Adaptable m z (t :> u :> v :> w :> x :> y :> z) where
--	adapt = lift . lift . lift . lift . lift . lift
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x :> y :> z))
--	, Lifting m u (Schematic Monad v (w :> x :> y :> z))
--	, Lifting m v (Schematic Monad w (x :> y :> z))
--	, Lifting m w (Schematic Monad x (y :> z))
--	, Lifting m x (Schematic Monad y z)
--	, Wrappable m y z
--	) => Adaptable m y (t :> u :> v :> w :> x :> y :> z) where
--	adapt = lift . lift . lift . lift . lift . wrap
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x :< y :< z))
--	, Lowering m u (Schematic Comonad v (w :< x :< y :< z))
--	, Lowering m v (Schematic Comonad w (x :< y :< z))
--	, Lowering m w (Schematic Comonad x (y :< z))
--	, Lowering m x (Schematic Comonad y z)
--	, Lowering m y z
--	) => Adaptable m (t :< u :< v :< w :< x :< y :< z) z where
--	adapt = lower . lower . lower . lower . lower . lower
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x :< y :< z))
--	, Lowering m u (Schematic Comonad v (w :< x :< y :< z))
--	, Lowering m v (Schematic Comonad w (x :< y :< z))
--	, Lowering m w (Schematic Comonad x (y :< z))
--	, Lowering m x (Schematic Comonad y z)
--	, Bringable m y z
--	) => Adaptable m (t :< u :< v :< w :< x :< y :< z) y where
--	adapt = bring . lower . lower . lower . lower . lower
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x :> y :> z :> f))
--	, Lifting m u (Schematic Monad v (w :> x :> y :> z :> f))
--	, Lifting m v (Schematic Monad w (x :> y :> z :> f))
--	, Lifting m w (Schematic Monad x (y :> z :> f))
--	, Lifting m x (Schematic Monad y (z :> f))
--	, Lifting m y (Schematic Monad z f)
--	, Lifting m z f
--	) => Adaptable m f (t :> u :> v :> w :> x :> y :> z :> f) where
--	adapt = lift . lift . lift . lift . lift . lift . lift
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x :> y :> z :> f))
--	, Lifting m u (Schematic Monad v (w :> x :> y :> z :> f))
--	, Lifting m v (Schematic Monad w (x :> y :> z :> f))
--	, Lifting m w (Schematic Monad x (y :> z :> f))
--	, Lifting m x (Schematic Monad y (z :> f))
--	, Lifting m y (Schematic Monad z f)
--	, Wrappable m z f
--	) => Adaptable m z (t :> u :> v :> w :> x :> y :> z :> f) where
--	adapt = lift . lift . lift . lift . lift . lift . wrap
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x :< y :< z :< f))
--	, Lowering m u (Schematic Comonad v (w :< x :< y :< z :< f))
--	, Lowering m v (Schematic Comonad w (x :< y :< z :< f))
--	, Lowering m w (Schematic Comonad x (y :< z :< f))
--	, Lowering m x (Schematic Comonad y (z :< f))
--	, Lowering m y (Schematic Comonad z f)
--	, Lowering m z f
--	) => Adaptable m (t :< u :< v :< w :< x :< y :< z :< f) f where
--	adapt = lower . lower . lower . lower . lower . lower . lower
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x :< y :< z :< f))
--	, Lowering m u (Schematic Comonad v (w :< x :< y :< z :< f))
--	, Lowering m v (Schematic Comonad w (x :< y :< z :< f))
--	, Lowering m w (Schematic Comonad x (y :< z :< f))
--	, Lowering m x (Schematic Comonad y (z :< f))
--	, Lowering m y (Schematic Comonad z f)
--	, Bringable m z f
--	) => Adaptable m (t :< u :< v :< w :< x :< y :< z :< f) z where
--	adapt = bring . lower . lower . lower . lower . lower . lower
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
--	, Lifting m u (Schematic Monad v (w :> x :> y :> z :> f :> h))
--	, Lifting m v (Schematic Monad w (x :> y :> z :> f :> h))
--	, Lifting m w (Schematic Monad x (y :> z :> f :> h))
--	, Lifting m x (Schematic Monad y (z :> f :> h))
--	, Lifting m y (Schematic Monad z (f :> h))
--	, Lifting m z (Schematic Monad f h)
--	, Lifting m f h
--	) => Adaptable m h (t :> u :> v :> w :> x :> y :> z :> f :> h) where
--	adapt = lift . lift . lift . lift . lift . lift . lift . lift
--
--instance
--	( Lifting m t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
--	, Lifting m u (Schematic Monad v (w :> x :> y :> z :> f :> h))
--	, Lifting m v (Schematic Monad w (x :> y :> z :> f :> h))
--	, Lifting m w (Schematic Monad x (y :> z :> f :> h))
--	, Lifting m x (Schematic Monad y (z :> f :> h))
--	, Lifting m y (Schematic Monad z (f :> h))
--	, Lifting m z (Schematic Monad f h)
--	, Wrappable m f h
--	) => Adaptable m f (t :> u :> v :> w :> x :> y :> z :> f :> h) where
--	adapt = lift . lift . lift . lift . lift . lift . lift . wrap
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x :< y :< z :< f :< h))
--	, Lowering m u (Schematic Comonad v (w :< x :< y :< z :< f :< h))
--	, Lowering m v (Schematic Comonad w (x :< y :< z :< f :< h))
--	, Lowering m w (Schematic Comonad x (y :< z :< f :< h))
--	, Lowering m x (Schematic Comonad y (z :< f :< h))
--	, Lowering m y (Schematic Comonad z (f :< h))
--	, Lowering m z (Schematic Comonad f h)
--	, Lowering m f h
--	) => Adaptable m (t :< u :< v :< w :< x :< y :< z :< f :< h) h where
--	adapt = lower . lower . lower . lower . lower . lower . lower . lower
--
--instance
--	( Lowering m t (Schematic Comonad u (v :< w :< x :< y :< z :< f :< h))
--	, Lowering m u (Schematic Comonad v (w :< x :< y :< z :< f :< h))
--	, Lowering m v (Schematic Comonad w (x :< y :< z :< f :< h))
--	, Lowering m w (Schematic Comonad x (y :< z :< f :< h))
--	, Lowering m x (Schematic Comonad y (z :< f :< h))
--	, Lowering m y (Schematic Comonad z (f :< h))
--	, Lowering m z (Schematic Comonad f h)
--	, Bringable m f h
--	) => Adaptable m (t :< u :< v :< w :< x :< y :< z :< f :< h) f where
--	adapt = bring . lower . lower . lower . lower . lower . lower . lower

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
