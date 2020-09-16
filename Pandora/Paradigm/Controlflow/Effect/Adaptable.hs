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

type Lifting t u = (Transformer Monad t, Liftable (Schematic Monad t), Covariant u)
type Wrappable t u = (Transformer Monad t, Pointable u)
type Flickable t u = (Transformer Comonad t, Covariant u)
type Bringable t u = (Transformer Comonad t, Extractable u)

instance Covariant t => Adaptable t t where
	adapt = identity

instance (Covariant (t :> u), Lifting t u) => Adaptable u (t :> u) where
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
	, Lifting t (Schematic Monad u v)
	, Lifting u v
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
	, Lifting t (Schematic Monad u (v :> w))
	, Lifting u (Schematic Monad v w)
	, Wrappable v w
	) => Adaptable v (t :> u :> v :> w) where
	adapt = lift . lift . wrap

instance
	( Covariant (t :> u :> v :> w)
	, Lifting t (Schematic Monad u v)
	, Lifting t (Schematic Monad u (v :> w))
	, Lifting u (Schematic Monad v w)
	, Lifting v w
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
	, Lifting t (Schematic Monad u (v :> w :> x))
	, Lifting u (Schematic Monad v (w :> x))
	, Lifting v (Schematic Monad w x)
	, Lifting w x
	) => Adaptable x (t :> u :> v :> w :> x) where
	adapt = lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x)
	, Lifting t (Schematic Monad u (v :> w :> x))
	, Lifting u (Schematic Monad v (w :> x))
	, Lifting v (Schematic Monad w x)
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
	, Lifting t (Schematic Monad u (v :> w :> x :> y))
	, Lifting u (Schematic Monad v (w :> x :> y))
	, Lifting v (Schematic Monad w (x :> y))
	, Lifting w (Schematic Monad x y)
	, Lifting x y
	) => Adaptable y (t :> u :> v :> w :> x :> y) where
	adapt = lift . lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x :> y)
	, Lifting t (Schematic Monad u (v :> w :> x :> y))
	, Lifting u (Schematic Monad v (w :> x :> y))
	, Lifting v (Schematic Monad w (x :> y))
	, Lifting w (Schematic Monad x y)
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
	, Lifting t (Schematic Monad u (v :> w :> x :> y :> z))
	, Lifting u (Schematic Monad v (w :> x :> y :> z))
	, Lifting v (Schematic Monad w (x :> y :> z))
	, Lifting w (Schematic Monad x (y :> z))
	, Lifting x (Schematic Monad y z)
	, Lifting y z
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z) where
	adapt = lift . lift . lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x :> y :> z)
	, Lifting t (Schematic Monad u (v :> w :> x :> y :> z))
	, Lifting u (Schematic Monad v (w :> x :> y :> z))
	, Lifting v (Schematic Monad w (x :> y :> z))
	, Lifting w (Schematic Monad x (y :> z))
	, Lifting x (Schematic Monad y z)
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
	, Lifting t (Schematic Monad u (v :> w :> x :> y :> z :> f))
	, Lifting u (Schematic Monad v (w :> x :> y :> z :> f))
	, Lifting v (Schematic Monad w (x :> y :> z :> f))
	, Lifting w (Schematic Monad x (y :> z :> f))
	, Lifting x (Schematic Monad y (z :> f))
	, Lifting y (Schematic Monad z f)
	, Lifting z f
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lift . lift . lift . lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x :> y :> z :> f)
	, Lifting t (Schematic Monad u (v :> w :> x :> y :> z :> f))
	, Lifting u (Schematic Monad v (w :> x :> y :> z :> f))
	, Lifting v (Schematic Monad w (x :> y :> z :> f))
	, Lifting w (Schematic Monad x (y :> z :> f))
	, Lifting x (Schematic Monad y (z :> f))
	, Lifting y (Schematic Monad z f)
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
	, Lifting t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
	, Lifting u (Schematic Monad v (w :> x :> y :> z :> f :> h))
	, Lifting v (Schematic Monad w (x :> y :> z :> f :> h))
	, Lifting w (Schematic Monad x (y :> z :> f :> h))
	, Lifting x (Schematic Monad y (z :> f :> h))
	, Lifting y (Schematic Monad z (f :> h))
	, Lifting z (Schematic Monad f h)
	, Lifting f h
	) => Adaptable h (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lift . lift . lift . lift . lift . lift . lift . lift

instance
	( Covariant (t :> u :> v :> w :> x :> y :> z :> f :> h)
	, Lifting t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
	, Lifting u (Schematic Monad v (w :> x :> y :> z :> f :> h))
	, Lifting v (Schematic Monad w (x :> y :> z :> f :> h))
	, Lifting w (Schematic Monad x (y :> z :> f :> h))
	, Lifting x (Schematic Monad y (z :> f :> h))
	, Lifting y (Schematic Monad z (f :> h))
	, Lifting z (Schematic Monad f h)
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
