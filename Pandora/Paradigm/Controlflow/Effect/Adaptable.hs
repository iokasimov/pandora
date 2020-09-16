{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category (identity, (.))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Pointable (Pointable)
import Pandora.Pattern.Functor.Extractable (Extractable)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Hoistable (Hoistable (hoist))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic)
import Pandora.Paradigm.Controlflow.Effect.Transformer (Transformer, wrap, flick, bring, (:>), (:<))

class Adaptable t u where
	{-# MINIMAL adapt #-}
	adapt :: t ~> u

type Layable t u = (Transformer Monad t, Liftable (Schematic Monad t), Covariant u)
type Wrappable t u = (Transformer Monad t, Pointable u)
type Flickable t u = (Transformer Comonad t, Covariant u)
type Bringable t u = (Transformer Comonad t, Extractable u)

instance Covariant t => Adaptable t t where
	adapt = identity

instance (Covariant (t :> u), Layable t u) => Adaptable u (t :> u) where
	adapt = lift

instance (Covariant (t :> u), Wrappable t u) => Adaptable t (t :> u) where
	adapt = wrap

instance (Covariant (t :> u), Flickable t u) => Adaptable (t :< u) u where
	adapt = flick

instance (Covariant (t :< u), Bringable t u) => Adaptable (t :< u) t where
	adapt = bring

instance
	( Covariant (t :> u :> v)
	, Transformer Monad t
	, Liftable (Schematic Monad t)
	, Covariant (Schematic Monad u v)
	, Wrappable u v
	) => Adaptable u (t :> u :> v) where
	adapt = lift . wrap

instance
	( Covariant (t :> u :> v)
	, Layable t (Schematic Monad u v)
	, Layable u v
	) => Adaptable v (t :> u :> v) where
	adapt = lift . lift

instance
	( Covariant (t :< u :< v)
	, Flickable t (Schematic Comonad u v)
	, Bringable u v
	) => Adaptable (t :< u :< v) u where
	adapt = bring . flick

instance
	( Covariant (t :< u :< v)
	, Flickable t (Schematic Comonad u v)
	, Flickable u v
	) => Adaptable (t :< u :< v) v where
	adapt = flick . flick

instance
	( Covariant (t :> u :> v :> w)
	, Liftable (Schematic Monad t)
	, Layable t (Schematic Monad u (v :> w))
	, Layable u (Schematic Monad v w)
	, Wrappable v w
	) => Adaptable v (t :> u :> v :> w) where
	adapt = lift . lift . wrap

instance
	( Covariant (t :> u :> v :> w)
	, Layable t (Schematic Monad u v)
	, Layable t (Schematic Monad u (v :> w))
	, Layable u (Schematic Monad v w)
	, Layable v w
	) => Adaptable w (t :> u :> v :> w) where
	adapt = lift . lift . lift

instance
	( Covariant (t :< u :< v :< w)
	, Flickable t (Schematic Comonad u (v :< w))
	, Flickable u (Schematic Comonad v w)
	, Bringable v w
	) => Adaptable (t :< u :< v :< w) v where
	adapt = bring . flick . flick

instance
	( Covariant (t :< u :< v :< w)
	, Flickable t (Schematic Comonad u v)
	, Flickable t (Schematic Comonad u (v :< w))
	, Flickable u (Schematic Comonad v w)
	, Flickable v w
	) => Adaptable (t :< u :< v :< w) w where
	adapt = flick . flick . flick

instance
	( Covariant (t :> u :> v :> w :> x)
	, Layable t (Schematic Monad u (v :> w :> x))
	, Layable u (Schematic Monad v (w :> x))
	, Layable v (Schematic Monad w x)
	, Layable w x
	) => Adaptable x (t :> u :> v :> w :> x) where
	adapt = lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x)
	, Layable t (Schematic Monad u (v :> w :> x))
	, Layable u (Schematic Monad v (w :> x))
	, Layable v (Schematic Monad w x)
	, Wrappable w x
	) => Adaptable w (t :> u :> v :> w :> x) where
	adapt = lift . lift . lift . wrap

instance
	( Covariant (t :< u :< v :< w :< x)
	, Flickable t (Schematic Comonad u (v :< w :< x))
	, Flickable u (Schematic Comonad v (w :< x))
	, Flickable v (Schematic Comonad w x)
	, Flickable w x
	) => Adaptable (t :< u :< v :< w :< x) x where
	adapt = flick . flick . flick . flick

instance
	( Covariant (t :< u :< v :< w :< x)
	, Flickable t (Schematic Comonad u (v :< w :< x))
	, Flickable u (Schematic Comonad v (w :< x))
	, Flickable v (Schematic Comonad w x)
	, Bringable w x
	) => Adaptable (t :< u :< v :< w :< x) w where
	adapt = bring . flick . flick . flick

instance
	( Covariant (t :> u :> v :> w :> x :> y)
	, Layable t (Schematic Monad u (v :> w :> x :> y))
	, Layable u (Schematic Monad v (w :> x :> y))
	, Layable v (Schematic Monad w (x :> y))
	, Layable w (Schematic Monad x y)
	, Layable x y
	) => Adaptable y (t :> u :> v :> w :> x :> y) where
	adapt = lift . lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x :> y)
	, Layable t (Schematic Monad u (v :> w :> x :> y))
	, Layable u (Schematic Monad v (w :> x :> y))
	, Layable v (Schematic Monad w (x :> y))
	, Layable w (Schematic Monad x y)
	, Wrappable x y
	) => Adaptable x (t :> u :> v :> w :> x :> y) where
	adapt = lift . lift . lift . lift . wrap

instance
	( Covariant (t :< u :< v :< w :< x :< y)
	, Flickable t (Schematic Comonad u (v :< w :< x :< y))
	, Flickable u (Schematic Comonad v (w :< x :< y))
	, Flickable v (Schematic Comonad w (x :< y))
	, Flickable w (Schematic Comonad x y)
	, Flickable x y
	) => Adaptable (t :< u :< v :< w :< x :< y) y where
	adapt = flick . flick . flick . flick . flick

instance
	( Covariant (t :< u :< v :< w :< x :< y)
	, Flickable t (Schematic Comonad u (v :< w :< x :< y))
	, Flickable u (Schematic Comonad v (w :< x :< y))
	, Flickable v (Schematic Comonad w (x :< y))
	, Flickable w (Schematic Comonad x y)
	, Bringable x y
	) => Adaptable (t :< u :< v :< w :< x :< y) x where
	adapt = bring . flick . flick . flick . flick

instance
	( Covariant (t :> u :> v :> w :> x :> y :> z)
	, Layable t (Schematic Monad u (v :> w :> x :> y :> z))
	, Layable u (Schematic Monad v (w :> x :> y :> z))
	, Layable v (Schematic Monad w (x :> y :> z))
	, Layable w (Schematic Monad x (y :> z))
	, Layable x (Schematic Monad y z)
	, Layable y z
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z) where
	adapt = lift . lift . lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x :> y :> z)
	, Layable t (Schematic Monad u (v :> w :> x :> y :> z))
	, Layable u (Schematic Monad v (w :> x :> y :> z))
	, Layable v (Schematic Monad w (x :> y :> z))
	, Layable w (Schematic Monad x (y :> z))
	, Layable x (Schematic Monad y z)
	, Wrappable y z
	) => Adaptable y (t :> u :> v :> w :> x :> y :> z) where
	adapt = lift . lift . lift . lift . lift . wrap

instance
	( Covariant (t :< u :< v :< w :< x :< y :< z)
	, Flickable t (Schematic Comonad u (v :< w :< x :< y :< z))
	, Flickable u (Schematic Comonad v (w :< x :< y :< z))
	, Flickable v (Schematic Comonad w (x :< y :< z))
	, Flickable w (Schematic Comonad x (y :< z))
	, Flickable x (Schematic Comonad y z)
	, Flickable y z
	) => Adaptable (t :< u :< v :< w :< x :< y :< z) z where
	adapt = flick . flick . flick . flick . flick . flick

instance
	( Covariant (t :< u :< v :< w :< x :< y :< z)
	, Flickable t (Schematic Comonad u (v :< w :< x :< y :< z))
	, Flickable u (Schematic Comonad v (w :< x :< y :< z))
	, Flickable v (Schematic Comonad w (x :< y :< z))
	, Flickable w (Schematic Comonad x (y :< z))
	, Flickable x (Schematic Comonad y z)
	, Bringable y z
	) => Adaptable (t :< u :< v :< w :< x :< y :< z) y where
	adapt = bring . flick . flick . flick . flick . flick

instance
	( Covariant (t :> u :> v :> w :> x :> y :> z :> f)
	, Layable t (Schematic Monad u (v :> w :> x :> y :> z :> f))
	, Layable u (Schematic Monad v (w :> x :> y :> z :> f))
	, Layable v (Schematic Monad w (x :> y :> z :> f))
	, Layable w (Schematic Monad x (y :> z :> f))
	, Layable x (Schematic Monad y (z :> f))
	, Layable y (Schematic Monad z f)
	, Layable z f
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lift . lift . lift . lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x :> y :> z :> f)
	, Layable t (Schematic Monad u (v :> w :> x :> y :> z :> f))
	, Layable u (Schematic Monad v (w :> x :> y :> z :> f))
	, Layable v (Schematic Monad w (x :> y :> z :> f))
	, Layable w (Schematic Monad x (y :> z :> f))
	, Layable x (Schematic Monad y (z :> f))
	, Layable y (Schematic Monad z f)
	, Wrappable z f
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lift . lift . lift . lift . lift . lift . wrap

instance
	( Covariant (t :< u :< v :< w :< x :< y :< z :< f)
	, Flickable t (Schematic Comonad u (v :< w :< x :< y :< z :< f))
	, Flickable u (Schematic Comonad v (w :< x :< y :< z :< f))
	, Flickable v (Schematic Comonad w (x :< y :< z :< f))
	, Flickable w (Schematic Comonad x (y :< z :< f))
	, Flickable x (Schematic Comonad y (z :< f))
	, Flickable y (Schematic Comonad z f)
	, Flickable z f
	) => Adaptable (t :< u :< v :< w :< x :< y :< z :< f) f where
	adapt = flick . flick . flick . flick . flick . flick . flick

instance
	( Covariant (t :< u :< v :< w :< x :< y :< z :< f)
	, Flickable t (Schematic Comonad u (v :< w :< x :< y :< z :< f))
	, Flickable u (Schematic Comonad v (w :< x :< y :< z :< f))
	, Flickable v (Schematic Comonad w (x :< y :< z :< f))
	, Flickable w (Schematic Comonad x (y :< z :< f))
	, Flickable x (Schematic Comonad y (z :< f))
	, Flickable y (Schematic Comonad z f)
	, Bringable z f
	) => Adaptable (t :< u :< v :< w :< x :< y :< z :< f) z where
	adapt = bring . flick . flick . flick . flick . flick . flick

instance
	( Covariant (t :> u :> v :> w :> x :> y :> z :> f :> h)
	, Layable t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
	, Layable u (Schematic Monad v (w :> x :> y :> z :> f :> h))
	, Layable v (Schematic Monad w (x :> y :> z :> f :> h))
	, Layable w (Schematic Monad x (y :> z :> f :> h))
	, Layable x (Schematic Monad y (z :> f :> h))
	, Layable y (Schematic Monad z (f :> h))
	, Layable z (Schematic Monad f h)
	, Layable f h
	) => Adaptable h (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lift . lift . lift . lift . lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x :> y :> z :> f :> h)
	, Layable t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
	, Layable u (Schematic Monad v (w :> x :> y :> z :> f :> h))
	, Layable v (Schematic Monad w (x :> y :> z :> f :> h))
	, Layable w (Schematic Monad x (y :> z :> f :> h))
	, Layable x (Schematic Monad y (z :> f :> h))
	, Layable y (Schematic Monad z (f :> h))
	, Layable z (Schematic Monad f h)
	, Wrappable f h
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lift . lift . lift . lift . lift . lift . lift . wrap

instance
	( Covariant (t :< u :< v :< w :< x :< y :< z :< f :< h)
	, Flickable t (Schematic Comonad u (v :< w :< x :< y :< z :< f :< h))
	, Flickable u (Schematic Comonad v (w :< x :< y :< z :< f :< h))
	, Flickable v (Schematic Comonad w (x :< y :< z :< f :< h))
	, Flickable w (Schematic Comonad x (y :< z :< f :< h))
	, Flickable x (Schematic Comonad y (z :< f :< h))
	, Flickable y (Schematic Comonad z (f :< h))
	, Flickable z (Schematic Comonad f h)
	, Flickable f h
	) => Adaptable (t :< u :< v :< w :< x :< y :< z :< f :< h) h where
	adapt = flick . flick . flick . flick . flick . flick . flick . flick

instance
	( Covariant (t :< u :< v :< w :< x :< y :< z :< f :< h)
	, Flickable t (Schematic Comonad u (v :< w :< x :< y :< z :< f :< h))
	, Flickable u (Schematic Comonad v (w :< x :< y :< z :< f :< h))
	, Flickable v (Schematic Comonad w (x :< y :< z :< f :< h))
	, Flickable w (Schematic Comonad x (y :< z :< f :< h))
	, Flickable x (Schematic Comonad y (z :< f :< h))
	, Flickable y (Schematic Comonad z (f :< h))
	, Flickable z (Schematic Comonad f h)
	, Bringable f h
	) => Adaptable (t :< u :< v :< w :< x :< y :< z :< f :< h) f where
	adapt = bring . flick . flick . flick . flick . flick . flick . flick

instance (Covariant u, Hoistable ((:>) t), Adaptable u u') => Adaptable (t :> u) (t :> u') where
	adapt = hoist adapt
